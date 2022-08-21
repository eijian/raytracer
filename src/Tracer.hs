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
import           Debug.Trace
import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as VU
import           NumericPrelude
import           System.IO
--import           System.Random.Mersenne as MT

import Ray.Algebra
import Ray.Geometry
import Ray.Light hiding (power)
import Ray.Mapper
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
      objs   = the list of Ojbect
      l      = depth level (max is 10)
      mate0  = Material to travel
      wl     = wavelength of the photon
      ray    = ray
      radest = method of estimating radiances
-}

tracePhoton :: V.Vector Object -> Int -> Material -> Material
   -> (Photon, RadEstimation) -> IO (V.Vector Photon)
tracePhoton !objs !l !mate_air !mate0 (!photon@(wl, ray@(_, vvec)), !radest)
  | l >= max_trace = return V.empty
  | otherwise = do
    case (calcIntersection ray objs) of
      Just is -> do
        let
          (t, (pos, nvec), (mate, surf), _) = is
          tracer = tracePhoton objs (l+1) mate_air
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
                    tracer mate' ((wl, initRay pos dir), PhotonMap)
                  Nothing -> return V.empty
        --if (uc == False || l > 0) && storePhoton mate == True
        if (radest == PhotonMap || l > 0) && storePhoton mate == True
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

traceRay :: PhotonFilter -> V.Vector Object -> V.Vector LightObject -> Int
  -> V.Vector PhotonMap -> Double -> Material -> Material -> Ray
  -> IO Radiance
traceRay !filter !objs !lgts !l !pmaps !radius !mate_air !mate0 !ray@(_, vvec) 
  | l >= max_trace = return radiance0
  | otherwise     = do
    case (calcIntersection ray objs) of
      Nothing            -> return radiance0
      Just is@(t, sfpt@(pos, nvec), (mate, surf), io) -> do
        let
          tracer = traceRay filter objs lgts (l+1) pmaps radius mate_air

        -- L_diffuse
        {-
        lrads <- V.forM lgts $ \lgt ->
          if (radest lgt) == PhotonMap
            then return Nothing
            else do
              lpoint <- randomPoint (lshape lgt)
              return $ calcRadiance lgt objs sfpt lpoint
        -}
        lrads <- V.mapM (getRadianceFromLight2 objs sfpt) lgts
        let
          di = (lrads `deepseq` foldl (+) radiance0 $ V.catMaybes lrads) +
               estimateRadiance radius filter pmaps is

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
          rad = bsdf mate surf cos1 di si ti
        return (tc <**> (emittance surf sfpt vvec) + rad)


estimateRadiance :: Double -> PhotonFilter -> V.Vector PhotonMap -> Intersection
  -> Radiance
estimateRadiance rmax filter pmaps (_, (pos, nvec), _, _) = mag *> rad
  where
    mag = one_pi / rmax * (power (pmaps V.! 0))
    rad = V.foldl (+) radiance0 $ V.map (estimateRadianceByMap rmax filter pos nvec) pmaps

estimateRadianceByMap :: Double -> PhotonFilter -> Position3 -> Direction3 -> PhotonMap
  -> Radiance
estimateRadianceByMap rmax filter pos nvec pmap
  | V.null ps = radiance0
  | otherwise = V.foldl (+) radiance0 rads -- 半径は指定したものを使う
  where
    ps = inradius pmap $ photonDummy pos
    f_wait = case filter of
      Nonfilter   -> filter_none rmax
      Conefilter  -> filter_cone rmax
      Gaussfilter -> filter_gauss rmax
    waits = ps `deepseq` V.map (\x -> f_wait (square (photonPos x - pos))) ps
    rads = V.zipWith (photonToRadiance nvec) waits ps

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

getRadianceFromLight2 :: V.Vector Object -> SurfacePoint -> LightObject
  -> IO (Maybe Radiance)
getRadianceFromLight2 objs sfpt (Object shp mp) =
  case lightSpecOnPoint mp sfpt (0.0, 0.0) of
    Nothing   -> return Nothing
    Just spec ->
      if (radest spec) == PhotonMap
        then return Nothing
        else do
          lpoint <- randomPoint shp
          let
            area = surfaceArea shp
          return $ calcRadiance spec objs sfpt lpoint area

calcRadiance :: LightSpec -> V.Vector Object -> SurfacePoint -> SurfacePoint
  -> Double -> Maybe Radiance
calcRadiance lgtspec objs (pos, nvec) (lpos, lnvec) area
  | ldir  == o3           = Nothing  -- 光源上の点
  | ldir <.> lnvec > 0.0  = Nothing  -- 光源が見えていない
  | lvec0 == Nothing      = Nothing  -- 光源方向のベクトルが異常
  | cos   <= 0.0          = Nothing  -- 光源が見えていない
  | is    == Nothing      = Nothing  -- 光源オブジェクトが無い
  | dist2 - t * t > 0.002 = Nothing  -- 光源の手前に物体がある
  | otherwise             = Just ((area * cos / dist2) *> rad)
  where
    ldir = lpos - pos
    lvec0 = normalize ldir
    lvec = fromJust lvec0
    cos = nvec <.> lvec
    is = calcIntersection (initRay pos lvec) objs
    dist2 = square ldir
    (t, _, _, _) = fromJust is
    rad = decayEmittance lgtspec (lnvec <.> (negate lvec))

  

---------------------------------
-- COMMON FUNCTIONS
---------------------------------

type Intersection = (Double, SurfacePoint, SurfaceChar, InOut)

calcIntersection :: Ray -> V.Vector Object -> Maybe Intersection
calcIntersection ray@(_, vvec) objs
  | length iss == 0  = Nothing
  | nvec0 == Nothing = Nothing
  | otherwise        = Just (t, sfpt, surfaceCharOnPoint mapper sfpt uv, io)
  where
    iss = V.mapMaybe (calcDistance ray) objs
    (t, uv, (Object shape mapper)) = V.foldl' nearer (V.head iss) (V.tail iss)
    pos = target t ray
    nvec0 = getNormal pos shape
    nvec = fromJust nvec0
    cos = nvec <.> vvec
    nvec' = if cos > 0.0 then negate nvec else nvec
    io = if cos > 0.0 then Out else In
    sfpt = (pos, nvec')
    nearer :: (Double, Vector2, Object) -> (Double, Vector2, Object)
      -> (Double, Vector2, Object)
    nearer d1@(t1, _, _) d2@(t2, _, _) = if t1 <= t2
      then d1
      else d2

calcDistance :: Ray -> Object -> Maybe (Double, Vector2, Object)
calcDistance ray obj@(Object shape _) = 
  case distance ray shape of
    Just dist -> Just (toDistance dist obj)
    Nothing   -> Nothing
  where
    toDistance :: (Vector2, Double, Shape) -> Object -> (Double, Vector2, Object)
    toDistance (uv, t, shape) (Object _ mapper) = (t, uv, (Object shape mapper))

{-
bsdf: BSDF
-}

bsdf :: Material -> Surface -> Double -> Radiance -> Radiance -> Radiance
  -> Radiance
bsdf (Material aldiff scat metal _ _ alspec) (Surface _ rough _ _) cos di si ti =
  i_de + i_mt
  where
    f = fresnelReflectanceColor alspec cos
    f2 = negate f
    i_de = if metal == 1.0
      then radiance0
      else ((1.0 - metal) *> (aldiff * f2)) <**>
           ((scat * one_pi) *> di + (1.0 - scat) *> ti)
    --i_mt = f <**> si
    i_mt = if metal == 1.0
      then f <**> si
      else (1.0 - rough) *> f <**> si

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
