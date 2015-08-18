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
traceRay 10 _ _ _ _ = Radiance 0 0 0
traceRay l pw pm os r
  | is == Nothing = Radiance 0 0 0
  | otherwise     = estimateRadiance pw n p m pis
  where
    is = calcIntersection r os
    (p, n, m) = fromJust is
    pin = PhotonInfo Red p ex3
    pis = kNearest pm nPhoton pin


estimateRadiance :: Double -> Direction3 -> Position3 -> Material
                 -> [PhotonInfo] -> Radiance
estimateRadiance pw n p m pis
  | r2 == 0   = Radiance 0 0 0
  | otherwise = fromJust (rad /> (sqpi2 * r2))
  where
    (r2, rad) = sumRadiance pw n p m pis

sumRadiance :: Double -> Direction3 -> Position3 -> Material -> [PhotonInfo]
            -> (Double, Radiance)
sumRadiance _ _ _ _ [] = (0, Radiance 0 0 0)
sumRadiance pw n p m ((PhotonInfo wl pos dir):pis) =
  (r2, rad) `addRadiance` (sumRadiance pw n p m pis)
  where
    cos = n <.> dir
    r2  = square (p - pos)
    rad = if cos > 0 then calcRadiance wl pw n m
                     else Radiance 0 0 0

addRadiance :: (Double, Radiance) -> (Double, Radiance) -> (Double, Radiance)
addRadiance (ar2, arad) (br2, brad) = (max_r2, arad + brad)
  where
    max_r2 = if ar2 > br2 then ar2 else br2

calcRadiance :: Wavelength -> Double -> Direction3 -> Material -> Radiance
calcRadiance Red   pw n (Material (Color r _ _)) = Radiance (pw * r) 0 0
calcRadiance Green pw n (Material (Color _ g _)) = Radiance 0 (pw * g) 0
calcRadiance Blue  pw n (Material (Color _ _ b)) = Radiance 0 0 (pw * b)

--
-- updated version of Photon mapping
--

traceRay'' :: Int -> Double -> KdTree Double PhotonInfo -> [Object] -> Ray
           -> Radiance
traceRay'' 10 _ _ _ _ = radiance0
traceRay'' l pw pmap objs r
  | is == Nothing = radiance0
  | otherwise     = estimateRadiance' pw pmap (fromJust is)
  where
    is = calcIntersection r objs

estimateRadiance' :: Double -> KdTree Double PhotonInfo -> Intersection
                  -> Radiance
estimateRadiance' pw pmap (p, n, m)
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

generateRay :: Position3 -> Position3 -> (Double, Double)
            -> (Direction3, Direction3) -> (Int, Int) -> Ray
generateRay e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    tgt   = o + ((sx * fromIntegral x) *> ex) + ((sy * fromIntegral y) *> ey)
    edir  = tgt - e 
    edir' = fromJust $ normalize edir

pi2 = 2 * pi :: Double  -- half steradian = 2 * pi

brdf :: Material -> Radiance -> Radiance
brdf m rad = (1.0 / pi2) *> ((diffSpec m) <**> rad)

