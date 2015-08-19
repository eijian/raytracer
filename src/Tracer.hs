{-# LANGUAGE NoImplicitPrelude #-}

--
-- Tracer
--

module Tracer where

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
import Ray.Physics
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
    ps = filter (isValidPhoton n) $ kNearest pmap nPhoton $ PhotonInfo Red p ex3
    rs = map (\(PhotonInfo _ x _) -> norm (x - p)) ps
    rmax = maximum rs
    rad = foldl (+) radiance0 $ map (photonInfoToRadiance pw) ps

isValidPhoton :: Direction3 -> PhotonInfo -> Bool
isValidPhoton n (PhotonInfo _ _ d) = n <.> d > 0

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
  | cos < 0       = radiance0
  | ld == Nothing = radiance0
  | longer > 0    = radiance0
  | otherwise  = (cos / sqrt sqDistLgt)  *> (getRadiance l p)
  where
    ldir = getDirection l p
    cos  = n <.> ldir
    ld = normalize ldir
    lray = initRay p $ fromJust ld
    is = calcIntersection lray objs
    (p', n', m') = fromJust is
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
