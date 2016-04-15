{-# LANGUAGE NoImplicitPrelude #-}

--
-- Tracer
--

module Tracer (
  readMap
, tracePhoton
, traceRay
, traceRay'
) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord
import Data.KdTree.Static
import NumericPrelude
--import Debug.Trace

import Ray.Algebra
import Ray.Geometry
import Ray.Object
import Ray.Light
import Ray.Material
--import Ray.Physics
import Ray.Optics

--
-- PARAMETERS

nPhoton :: Int
nPhoton = 200

sqpi2 :: Double
sqpi2 = 2 * pi * pi    -- pi x steradian (2pi) for half sphere

--
--

tracePhoton :: [Object] -> Photon -> IO [PhotonCache]
tracePhoton os (wl, r) = do
  let iss = filter (\x -> fst x > nearly0) (concat $ map (calcDistance r) os)
      (t, s) = head $ sortBy (comparing fst) iss
  return [(wl, initRay (target t r) (getDir r))]

-----
-- RAY TRACING WITH PHOTON MAP
-----

traceRay :: Int -> Double -> KdTree Double PhotonInfo -> [Object] -> Ray
         -> Radiance
traceRay 10 _ _ _ _ = radiance0
traceRay l pw pmap objs r
  | is == Nothing = radiance0
  | otherwise     = estimateRadiance pw pmap (fromJust is)
  where
    is = calcIntersection r objs

estimateRadiance :: Double -> KdTree Double PhotonInfo -> Intersection
                 -> Radiance
estimateRadiance pw pmap (p, n, m)
  | ps == []  = radiance0
  | otherwise = (1.0 / (pi * rmax * rmax)) *> (brdf m rad)
  where
    ps = filter (isValidPhoton p n) $ kNearest pmap nPhoton $ photonDummy p
    rs = map (\x -> norm ((photonPos x) - p)) ps
    rmax = maximum rs
    rad = sumRadiance1 pw rmax rs ps

radius2 :: Double
radius2 = 0.1 * 0.1

isValidPhoton :: Position3 -> Direction3 -> PhotonInfo -> Bool
--isValidPhoton p n ph = n <.> (photonDir ph) > 0
isValidPhoton p n ph = n <.> (photonDir ph) > 0 &&
                       square (p - photonPos ph) < radius2

-- filtering:
--   sumRadiance1  non filter
--   sumRadiance2  cone filter
--   sumRadiance3  gauss filter

-- Normal (non filter)
sumRadiance1 :: Double -> Double -> [Double] -> [PhotonInfo] -> Radiance
sumRadiance1 pw rmax rs ps = foldl (+) radiance0 rads
  where
    rads = map (photonInfoToRadiance pw) ps

-- Cone filter
k_cone :: Double
k_cone = 1.1

fac_k :: Double
fac_k = 1.0 - 2.0 / (3.0 * k_cone)

sumRadiance2 :: Double -> Double -> [Double] -> [PhotonInfo] -> Radiance
sumRadiance2 pw rmax rs ps = foldl (+) radiance0 rads
  where
    wt = map (waitCone (pw / fac_k) rmax) rs
    rads = zipWith (photonInfoToRadiance) wt ps

waitCone :: Double -> Double -> Double -> Double
waitCone pw rmax dp = pw * (1.0 - dp / (k_cone * rmax))

-- Gauss filter

alpha :: Double
alpha = 0.918

beta :: Double
beta  = 1.953

e_beta :: Double
e_beta = 1.0 - exp (-beta)

sumRadiance3 :: Double -> Double -> [Double] -> [PhotonInfo] -> Radiance
sumRadiance3 pw rmax rs ps = foldl (+) radiance0 rads
  where
    wt = map (waitGauss pw rmax) rs
    rads = zipWith (photonInfoToRadiance) wt ps

waitGauss :: Double -> Double -> Double -> Double
waitGauss pw rmax dp = pw * alpha * (1.0 - e_r / e_beta)
  where
    e_r = 1.0 - exp (-beta * dp * dp / (2.0 * rmax * rmax))

------
-- CLASICAL RAY TRACING
------

traceRay' :: Int -> [Light] -> [Object] -> Ray -> Radiance
traceRay' l lgts objs r
  | is == Nothing = radiance0
  | otherwise     = brdf m radDiff
  where
    is = calcIntersection r objs
    (p, n, m) = fromJust is
    radDiff = foldl (+) radiance0 $ map (getRadianceFromLight objs p n) lgts

getRadianceFromLight :: [Object] -> Position3 -> Direction3 -> Light
                     -> Radiance
getRadianceFromLight objs p n l
  | cos0 < 0      = radiance0
  | ld == Nothing = radiance0
  | longer > 0    = radiance0
  | otherwise  = (cos0 / sqrt sqDistLgt)  *> (getRadiance l p)
  where
    ldir = getDirection l p
    cos0 = n <.> ldir
    ld = normalize ldir
    lray = initRay p $ fromJust ld
    is = calcIntersection lray objs
    (p', _, _) = fromJust is
    sqDistLgt = square ldir
    sqDistObj = square (p' - p)
    longer = sqDistLgt - sqDistObj -- compare distances of light and obj

---------------------------------
-- COMMON FUNCTIONS
---------------------------------

type Intersection = (Position3, Direction3, Material)

calcIntersection :: Ray -> [Object] -> Maybe Intersection
calcIntersection r os
  | iss == [] = Nothing
  | otherwise = Just (p, fromJust (getNormal p s), m)
  where
    p = target t r
    iss = filter (\x -> fst x > nearly0) (concat $ map (calcDistance r) os)
    (t, (Object s m)) = head $ sortBy (comparing fst) iss

calcDistance :: Ray -> Object -> [(Double, Object)]
calcDistance r o@(Object s m) = zip ts (replicate (length ts) o)
  where
    ts = distance r s

pi2 = 2 * pi :: Double  -- half steradian = 2 * pi

brdf :: Material -> Radiance -> Radiance
brdf m rad = (1.0 / pi2) *> ((diffSpec m) <**> rad)

readMap :: IO (Double, KdTree Double PhotonInfo)
readMap = do
  np' <- getLine
  pw' <- getLine
  let np = read np' :: Int
  let pw = read pw' :: Double
  pcs <- forM ([1..np]) $ \i -> do
    l <- getLine
    return $ (read l :: PhotonCache)
  let pmap = build infoToPointList (map convertToInfo pcs)
  return (pw, pmap)
