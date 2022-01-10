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
--, traceRay'
) where

import           Control.DeepSeq
--import           Control.DeepSeq.Generics (genericRnf)
import           Control.Monad
--import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.List hiding (sum)
import           Data.Ord
--import           Debug.Trace
import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as VU
import           NumericPrelude
--import           System.Random.Mersenne as MT

import Ray.Algebra
import Ray.Geometry
import Ray.Light hiding (power)
import Ray.Material
import Ray.Object
import Ray.Optics
import Ray.Physics
import Ray.Surface hiding (alpha)

import PhotonMap
import Screen

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

tracePhoton :: Bool -> V.Vector Object -> Int -> Material -> Material -> Photon
  -> IO (V.Vector Photon)
tracePhoton !uc !objs !l !mate_air !mate0 !photon@(wl, ray@(_, vvec))
  | l >= max_trace = return V.empty
  | otherwise = do
    case (calcIntersection ray objs) of
      Just is -> do
        let
          (t, pos, nvec, mate, surf, _) = is
          tracer = tracePhoton uc objs (l+1) mate_air
        ref <- do
            -- フォトンが物体に到達するまでに透過率が低く吸収される場合あり
            -- transmission ** t の確率で到達する
            let
              tr = (selectWavelength wl (transmittance mate0)) ** t
            i <- russianRouletteBinary tr
            if i == False
              then return V.empty
              else do
                let
                  eta = relativeIorWavelength (ior mate0) (ior mate) wl
                nextdir <- nextDirection mate surf eta nvec photon
                case nextdir of
                  Just (dir, mf) -> do
                    let mate' = if mf == True then mate0 else mate
                    tracer mate' (wl, initRay pos dir)
                  Nothing -> return V.empty
        if (uc == False || l > 0) && storePhoton mate == True
          then return $ V.cons (wl, (pos, vvec)) ref
          else return ref
      Nothing -> return V.empty


{- |
nextDirection

-- OUT: dir  next ray direction. if dir is None, the photon is absorbed.
--      T/F  true = reflection, false = rafraction
-}

nextDirection :: Material -> Surface -> Double -> Direction3 -> Photon
  -> IO (Maybe (Direction3, Bool))
nextDirection mate surf eta nvec (wl, (_, vvec)) = do
  nvec' <- microfacetNormal nvec vvec surf (metalness mate)
  case nvec' of
    Nothing    -> return Nothing
    Just nvec' -> do
      let
        (rdir, cos1) = specularReflection nvec' vvec
      pb <- photonBehavior mate cos1 wl
      case pb of
        SpecularReflection -> return $ Just (rdir, True)   -- 鏡面反射
        Absorption         -> return Nothing               -- 吸収
        DiffuseReflection  -> do
          df <- diffuseReflection nvec
          return $ Just (df, True)                         -- 拡散反射
        SpecularTransmission -> do                         -- 鏡面透過
          let (tdir, _) = specularRefraction nvec' vvec eta cos1
          case tdir of
            Just tdir -> return $ Just (tdir, False)
            Nothing   -> return Nothing

-----
-- RAY TRACING WITH PHOTON MAP
-----

traceRay :: Screen -> Bool -> V.Vector Object -> V.Vector Light -> Int
  -> PhotonMap -> Double -> Material -> Material -> Ray -> IO Radiance
traceRay !scr !uc !objs !lgts !l !pmap !radius !mate_air !mate0 !ray@(_, vvec) 
  | l >= max_trace = return radiance0
  | otherwise     = do
    case (calcIntersection ray objs) of
      Nothing            -> return radiance0
      Just is@(t, pos, nvec, mate, surf, io) -> do
        let
          tracer = traceRay scr uc objs lgts (l+1) pmap radius mate_air

        -- L_diffuse
        lpos <- V.mapM (randomPoint.lshape) lgts
        let
          di' = if uc 
            then foldl (+) radiance0 $ V.map (getRadianceFromLight objs pos nvec) (V.zip lgts lpos)
            else radiance0
          di = di' + estimateRadiance radius scr pmap is

        -- L_spec
        nvec' <- microfacetNormal nvec vvec surf (metalness mate)
        (si, _rvec, ti, _tvec, cos1, _eta) <- case nvec' of
          Nothing    -> do
            let
              (rvec, cos1) = specularReflection nvec vvec
            return (radiance0, rvec, radiance0, Nothing, cos1, 1.0)
          Just nvec' -> do
            let
              (rvec, cos1) = specularReflection nvec' vvec
            si <- if (reflect mate cos1) == True
              then tracer mate0 (pos, rvec)
              else return radiance0

            -- L_trans
            let
              eta = relativeIorAverage (ior mate0) (ior mate)
              (tvec, _) = specularRefraction nvec' vvec eta cos1
            ti <- case tvec of
              Nothing   -> return radiance0
              Just tvec ->
               if refract mate == True
                  then do
                    let mate' = if io == In then mate else mate_air
                    tracer mate' (pos, tvec)
                  else return radiance0
            return (si, rvec, ti, tvec, cos1, eta)

        let
          tc = expColor (transmittance mate0) t
          rad = bsdf mate cos1 di si ti
        return (tc <**> (sr_half *> (emittance surf pos vvec) + rad))


estimateRadiance :: Double -> Screen -> PhotonMap -> Intersection -> Radiance
estimateRadiance rmax scr pmap (_, pos, nvec, _, _, _)
  | V.null ps = radiance0
  | otherwise = (one_pi / rmax * (power pmap)) *> rad -- 半径は指定したものを使う
  where
    ps = inradius pmap $ photonDummy pos
    f_wait = case (pfilter scr) of
      Nonfilter   -> filter_none rmax
      Conefilter  -> filter_cone rmax
      Gaussfilter -> filter_gauss rmax
    waits = ps `deepseq` V.map (\x -> f_wait (square (photonPos x - pos))) ps
    rads = V.zipWith (photonToRadiance nvec) waits ps
    rad = V.foldl (+) radiance0 rads

-- filtering:
--   sumRadiance1  none filter
--   sumRadiance2  cone filter
--   sumRadiance3  gauss filter

-- Normal (none filter)
filter_none :: Double -> Double -> Double
filter_none _ _ = 1.0

-- Cone filter
k_cone :: Double
k_cone = 1.1

fac_k :: Double
fac_k = 1.0 - 2.0 / (3.0 * k_cone)

filter_cone :: Double -> Double-> Double
filter_cone rmax d = if d' > 1.0 then 0.0 else (1.0 - d') / fac_k
  where
    d' = sqrt (d / rmax) / k_cone

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

getRadianceFromLight :: V.Vector Object -> Position3 -> Direction3
  -> (Light, Position3) -> Radiance
getRadianceFromLight objs pos _nvec (lgt, lpos)
  | lvec == Nothing        = radiance0
  | is   == Nothing        = radiance0
  | dist2 - dist2' > 0.002 = radiance0   -- 光源の前に物体がある
  | otherwise              = rad
  where
    (dist2, lvec, rad) = getRadiance lgt lpos pos
    is = calcIntersection (initRay pos (fromJust lvec)) objs
    (_, pos', _, _, _, _) = fromJust is
    dist2' = square (pos' - pos)

---------------------------------
-- COMMON FUNCTIONS
---------------------------------

data InOut = In | Out deriving (Eq, Show)
type Intersection = (Double, Position3, Direction3, Material, Surface, InOut)

calcIntersection :: Ray -> V.Vector Object -> Maybe Intersection
calcIntersection ray@(_, vvec) objs
  | iss == [] = Nothing
  | otherwise =
    case nvec of
      Just nvec -> if nvec <.> vvec > 0.0
        then Just (t, pos, negate nvec, mate, surf, Out)
        else Just (t, pos, nvec       , mate, surf, In)
      Nothing -> Nothing
  where
    iss = filter (\x -> fst x > nearly0) (concat $ V.map (calcDistance ray) objs)
    (t, (Object shape mate surf)) = head $ sortBy (comparing fst) iss
    pos = target t ray
    nvec = getNormal pos shape

calcDistance :: Ray -> Object -> [(Double, Object)]
calcDistance ray obj@(Object shape _ _) = zip ts (replicate (length ts) obj)
  where
    ts = distance ray shape

{-
bsdf: BSDF
-}

bsdf :: Material -> Double -> Radiance -> Radiance -> Radiance -> Radiance
bsdf (Material aldiff scat metal _ _ alspec) cos di si ti =
  i_de + i_mt
  where
    f = fresnelReflectanceColor alspec cos
    f2 = negate f
    i_de = if metal == 1.0
      then radiance0
      else ((1.0 - metal) *> (aldiff * f2)) <**>
           ((scat * one_pi) *> di + (1.0 - scat) *> ti)
    i_mt = f <**> si

{-
schlickG: 幾何減衰Gの計算で用いるschlick近似式
  Gを用いるのは当面保留のためコメントアウト
-}
{-
schlickG :: Direction3 -> Direction3 -> Double -> Double
schlickG nvec vvec k = cos / (cos * (1.0 - k) + k)
  where
    cos = nvec <.> vvec
-}
