{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

--
-- Tracer
--

module Tracer (
  readMap
, tracePhoton
, traceRay
, traceRay'
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.List hiding (sum)
import           Data.Ord
--import qualified Data.KdTree.Static as KT
--import qualified Data.KdTree.Dynamic as KT
import           Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           NumericPrelude
import           System.Random.Mersenne as MT

import Ray.Algebra
import Ray.Geometry
import Ray.Object
import Ray.Light
import Ray.Material
import Ray.Physics
import Ray.Optics
import Ray.Surface hiding (alpha, diffuseness, metalness, reflectance)

import PhotonMap
import Screen
import Scene

--
-- CONSTANTS
--

max_trace :: Int
max_trace = 10

--

{-
  tracePhoton:
    In:
      uc = flag of the use classic ray-tracing for direct illumination
      os = the list of Ojbect
      l  = depth level (max is 10)
      m0 = Material to travel
      wl = wavelength of the photon
      r  = ray
-}

tracePhoton :: Bool -> V.Vector Object -> Int -> Material -> Photon
            -> IO (V.Vector Photon)
tracePhoton !uc !os !l !m0 !ph@(wl, r@(_, rd))
  | l >= max_trace = return V.empty
  | otherwise = do
    case (calcIntersection r os) of
      Just is -> do
        let
          (t, p, n, m, io) = is
          sf = surface m
          tracer = tracePhoton uc os (l+1)
        ref <- case sf of
          (Simple _ _ diff _ _ _) -> do
            i <- russianRoulette [diff] 
            if i > 0
              then reflectDiff tracer m0 ph is
              else reflectSpec tracer m0 ph is
          (TS _ _ _ _ rough _ _)  -> do
            -- フォトンが物体に到達するまでに透過率が低く吸収される場合あり
            -- transmission ** t の確率で到達する
            let
              tr = (selectWavelength wl (transmittance m0)) ** t
            i <- russianRoulette [tr]
            if i == 0
              then return V.empty
              else do
                let
                  eta = relativeIorWavelength (ior m0) (ior m) wl
                nextdir <- nextDirection sf eta n ph
                case nextdir of
                  Just (dir, mf) -> do
                    let mate = if mf == True then m0 else m
                    tracer mate (wl, initRay p dir)
                  Nothing -> return V.empty
          _ -> return V.empty

        if (uc == False || l > 0) && storePhoton sf == True
          then return $ V.cons (wl, (p, rd)) ref
          else return ref
      Nothing -> return V.empty

reflectDiff :: (Material -> Photon -> IO (V.Vector Photon))
  -> Material -> Photon -> Intersection
  -> IO (V.Vector Photon)
reflectDiff tracer m0 (wl, _) (t, p, n, m, _) = do
  i <- russianRoulette [albedoDiff (surface m) wl]
  if i > 0
    then do  -- diffuse reflection
      dr <- diffuseReflection n
      tracer m0 (wl, initRay p dr)
    else return V.empty -- absorption

reflectSpec :: (Material -> Photon -> IO (V.Vector Photon))
  -> Material -> Photon -> Intersection
  -> IO (V.Vector Photon)
reflectSpec tracer m0 ph@(wl, (_, ed)) is@(t, p, n, m, _) = do
  nvec' <- if rough (surface m) == 0.0
    then return n
    else distributedNormal n (powerGlossy $ surface m)
  let
    (rdir, cos1) = specularReflection nvec' ed
    f = schlick (albedoSpec (surface m) wl) cos1
  j <- russianRoulette [f]
  if j > 0
    then tracer m0 (wl, initRay p rdir)
    else do
      if (selectWavelength wl $ ior m) == 0.0
        then return V.empty   -- non transparency
        else reflectTrans tracer m0 ph (t, p, nvec', m, In) cos1

reflectTrans :: (Material -> Photon -> IO (V.Vector Photon))
  -> Material -> Photon -> Intersection -> Double
  -> IO (V.Vector Photon)
reflectTrans tracer m0 (wl, (_, ed)) (t, pos, nvec, m, _) c0 = do
  let
    eta = relativeIorWavelength (ior m0) (ior m) wl
    (tdir, cos2) = specularRefraction nvec ed eta c0
  case tdir of
    Just tdir ->  do
      let m0' = if tdir <.> nvec < 0.0 then m else m_air
      tracer m0' (wl, initRay pos tdir)
    Nothing -> return V.empty

{- |
nextDirection

-- OUT: dir  next ray direction. if dir is None, the photon is absorbed.
--      T/F  true = reflection, false = rafraction
-}

nextDirection :: Surface -> Double -> Direction3 -> Photon
  -> IO (Maybe (Direction3, Bool))
nextDirection sf@(TS aldiff alspec scat _ _ _ pow) eta nvec (wl, (_, vvec)) = do
  nvec' <- if rough sf == 0.0
    then return nvec
    else distributedNormal nvec pow
  let (rdir, cos1) = specularReflection nvec' vvec
  --rdir <- reflectionGlossy nvec rdir0 (powerGlossy sf)
  let
    --hvec = fromJust $ normalize (rdir - vvec)
    --(tdir, cos2) = specularRefraction hvec vvec eta cos1
    (tdir, cos2) = specularRefraction nvec' vvec eta cos1
    cos = if cos1 < cos2 then cos1 else cos2
  pb <- photonBehavior sf cos wl
  --putStrLn ("ph=" ++ show pb)
  case pb of
    SpecularReflection -> return $ Just (rdir, True)   -- 鏡面反射
    Absorption         -> return Nothing               -- 吸収
    DiffuseReflection  -> do
      df <- diffuseReflection nvec
      return $ Just (df, True)                         -- 拡散反射
    SpecularTransmission ->                            -- 鏡面透過
      case tdir of
        Just tdir -> return $ Just (tdir, False)
        Nothing   -> return Nothing
nextDirection (Simple _ _ _ _ _ _) _ _ _ = return Nothing
nextDirection _ _ _ _ = return Nothing


-----
-- RAY TRACING WITH PHOTON MAP
-----

traceRay :: Screen -> Bool -> V.Vector Object -> V.Vector Light -> Int
  -> PhotonMap -> Double -> Material -> Ray -> IO Radiance
traceRay !scr !uc !objs !lgts !l !pmap !radius !m0 !r@(_, vvec) 
  | l >= max_trace = return radiance0
  | otherwise     = do
    case (calcIntersection r objs) of
      Nothing            -> return radiance0
      Just (t, p, n, m, io) -> do
        let
          tracer = traceRay scr uc objs lgts (l+1) pmap radius

        -- L_diffuse
        r1 <- MT.randomIO :: IO Double
        r2 <- MT.randomIO :: IO Double
        let
          di' = if uc 
            then foldl (+) radiance0 $ V.map (getRadianceFromLight objs p n (r1, r2)) lgts
            else radiance0
          di = di' + estimateRadiance radius scr pmap (t, p, n, m, io)

        -- L_spec
        let
          sf = surface m

        nvec' <- if rough sf == 0.0
          then return n
          else distributedNormal n (powerGlossy sf)
        let
          (rdir, cos1) = specularReflection nvec' vvec
          --(rdir0, cos1) = specularReflection n vvec
        --rdir <- reflectionGlossy n rdir0 (powerGlossy sf)
        si <- if (reflect sf cos1) == True
          then tracer m0 (p, rdir)
          else return radiance0

        -- L_trans
        let
          eta = relativeIorAverage (ior m0) (ior m)
          --hvec = fromJust $ normalize (rdir - vvec)
          --(tdir, cos2) = specularRefraction hvec (getDir r) eta cos1
          (tdir, cos2) = specularRefraction nvec' (getDir r) eta cos1
          --cos = if cos1 < cos2 then cos1 else cos2
          cos = cos1
        ti <- case tdir of
          Nothing   -> return radiance0
          Just tdir ->
            if refract sf cos1 == True
              then do
                let m0' = if io == In then m else m_air
                tracer m0' (p, tdir)
              else return radiance0

        let
          tc = expColor (transmittance m0) t
          rad = bsdf sf n vvec rdir tdir cos eta di si ti
        return (tc <**> (sr_half *> emittance m + rad))

{-

    ti <- if f' == black || ior1 == 0.0
      then return radiance0
      else do
        let
          ior0 = averageIor m0
          (tdir, cos2) = specularRefraction ior0 ior1 cos0 (getDir r) n
          m0' = if tdir <.> n < 0.0 then m else m_air
        traceRay scr m0' (l+1) pmap objs lgts (initRay p tdir)
    si `deepseq` ti `deepseq` return 
      (sr_half    *> emittance m +
       df         *> brdf m (di + ii) +
       (1.0 - df) *> (f <**> si + (1.0 - mt) *> f' <**> ti))
  where
    df = diffuseness m
    mt = metalness m
    f = reflectionIndex (specularRefl m) cos0
    f' = negateColor f                         -- this means '1 - f'
    ior1 = averageIor m
-}

--ps0 :: V.Vector PhotonInfo
--ps0 = V.fromList [PhotonInfo Red o3 ex3, PhotonInfo Green ex3 ey3]

estimateRadiance :: Double -> Screen -> PhotonMap -> Intersection -> Radiance
estimateRadiance rmax scr pmap (t, p, n, m, _)
  | V.null ps = radiance0
  | otherwise = (one_pi / rmax * (power pmap)) *> rad -- 半径は指定したものを使う
  where
    --ps = (nearest pmap) $ photonDummy p
    --ps = V.fromList $ (inradius pmap) $ photonDummy p
    ps = inradius pmap $ photonDummy p
    f_wait = case (pfilter scr) of
      Nonfilter   -> filter_none rmax
      Conefilter  -> filter_cone rmax
      Gaussfilter -> filter_gauss rmax
    wts = ps `deepseq` V.map (\x -> f_wait (square (photonPos x - p))) ps
    rds = V.zipWith (photonToRadiance n) wts ps
    rad = V.foldl (+) radiance0 rds

-- filtering:
--   sumRadiance1  none filter
--   sumRadiance2  cone filter
--   sumRadiance3  gauss filter

-- Normal (none filter)

filter_none :: Double -> Double -> Double
filter_none _ _ = 1.0

{-
sumRadiance1 :: Position3 -> Direction3 -> Double -> Double -> [Double]
             -> [PhotonInfo] -> Radiance
sumRadiance1 p n pw rmax _ ps = rds `deepseq` rad
  where
    ps' = filter adopt ps
    rds = map (photonInfoToRadiance n pw) ps'
    rad = foldl (+) radiance0 rds
    adopt :: PhotonInfo -> Bool
    adopt ph = square (p - photonPos ph) < rmax * rmax
-}

-- Cone filter
k_cone :: Double
k_cone = 1.1

fac_k :: Double
fac_k = 1.0 - 2.0 / (3.0 * k_cone)

filter_cone :: Double -> Double-> Double
filter_cone rmax d = if d' > 1.0 then 0.0 else (1.0 - d') / fac_k
  where
    d' = sqrt (d / rmax) / k_cone

{-
sumRadiance2 :: Position3 -> Direction3 -> Double -> Double -> [Double]
             -> [PhotonInfo] -> Radiance
sumRadiance2 _ n pw rmax rs ps = rds `deepseq` rad
  where
    wt = map (waitCone (pw / fac_k) rmax) rs
    rds = zipWith (photonInfoToRadiance n) wt ps
    rad = foldl (+) radiance0 rds

waitCone :: Double -> Double -> Double -> Double
waitCone pw radius dp
  | d > 1.0   = 0.0
  | otherwise = pw * (1.0 - d)
  where
    d = dp / (k_cone * radius)
-}

-- Gauss filter

alpha :: Double
alpha = 0.918

beta :: Double
beta  = 1.953

e_beta :: Double
e_beta = 1.0 - exp (-beta)

corr :: Double
corr = 0.5

filter_gauss :: Double -> Double -> Double
filter_gauss rmax d = if e_r > e_beta then 0.0 else alpha * (1.0 - e_r / e_beta) + corr
  where
    e_r = 1.0 - exp (-beta * d / (rmax * 2.0))
{-
sumRadiance3 :: Position3 -> Direction3 -> Double -> Double -> [Double]
             -> [PhotonInfo] -> Radiance
sumRadiance3 _ n pw rmax rs ps = rds `deepseq` rad
  where
    wt = map (waitGauss pw rmax) rs
    rds = zipWith (photonInfoToRadiance n) wt ps
    rad = foldl (+) radiance0 rds
    waitGauss :: Double -> Double -> Double -> Double
    waitGauss p r dp
      | wp < 0.0 = 0.0
      | otherwise = wp
      where
        e_r = 1.0 - exp (-beta * dp * dp / (2.0 * r))
        wp  = p * alpha * (1.0 - e_r / e_beta)
-}

------
-- CLASICAL RAY TRACING
------

traceRay' :: Screen -> Int -> V.Vector Light -> V.Vector Object -> Ray -> IO Radiance
traceRay' !scr l lgts objs r
  | is == Nothing = return radiance0
  | otherwise     = return (
                    sr_half *> emittance m
                  + brdf m (radDiff + (ambient scr))
                  )
  where
    is = calcIntersection r objs
    (t, p, n, m, io) = fromJust is
    radDiff = foldl (+) radiance0 $ V.map (getRadianceFromLight objs p n (0.0, 0.0)) lgts

getRadianceFromLight :: V.Vector Object -> Position3 -> Direction3 -> (Double, Double)
                     -> Light -> Radiance
getRadianceFromLight objs p n blur l = sum $ zipWith (*>) coss $ getRadiance l dists
  where
    (dists, coss) = unzip $ illuminated objs p n $ getDirection l p blur

illuminated :: V.Vector Object -> Position3 -> Direction3 -> [Direction3]
            -> [(Double, Double)]
illuminated _ _ _ []          = []
illuminated os p n (ld:lds)
  | ld' == Nothing              = illuminated os p n lds
  | cos0 < 0.0                  = illuminated os p n lds
  | is == Nothing               = illuminated os p n lds
  | sqLdist - sqOdist > 0.002   = illuminated os p n lds
  | otherwise                   = (sqLdist, cos0 * cos0):illuminated os p n lds
  where
    ld'  = normalize ld
    ld'' = fromJust ld'
    cos0 = n <.> ld''
    lray = initRay p ld''
    is = calcIntersection lray os
    (_, p', _, _, _) = fromJust is
    sqLdist = square ld
    sqOdist = square (p' - p)

---------------------------------
-- COMMON FUNCTIONS
---------------------------------

data InOut = In | Out deriving Eq
type Intersection = (Double, Position3, Direction3, Material, InOut)

calcIntersection :: Ray -> V.Vector Object -> Maybe Intersection
calcIntersection ray@(_, raydir) objs
  | iss == [] = Nothing
  | otherwise =
    case nvec of
      Just nvec -> if nvec <.> raydir > 0.0
        then Just (t, pos, negate nvec, mate, Out)
        else Just (t, pos, nvec, mate, In)
      Nothing -> Nothing
  where
    iss = filter (\x -> fst x > nearly0) (concat $ V.map (calcDistance ray) objs)
    (t, (Object shape mate)) = head $ sortBy (comparing fst) iss
    pos = target t ray
    nvec = getNormal pos shape

calcDistance :: Ray -> Object -> [(Double, Object)]
calcDistance r o@(Object s _) = zip ts (replicate (length ts) o)
  where
    ts = distance r s

brdf :: Material -> Radiance -> Radiance
--brdf m rad = (diffuseness m / pi2) *> ((reflectance m) <**> rad)
brdf m rad = one_pi *> ((reflectance m) <**> rad)

