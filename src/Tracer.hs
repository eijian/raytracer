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
import Data.List hiding (sum)
import Data.Ord
import Data.KdTree.Static
import NumericPrelude
--import Debug.Trace
import System.IO

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

radius2 :: Double
radius2 = 0.2 * 0.2

sqpi2 :: Double
sqpi2 = 2 * pi * pi    -- pi x steradian (2pi) for half sphere

--
--

tracePhoton :: [Object] -> Int -> Photon -> IO [PhotonCache]
tracePhoton os l (wl, r) = do
  let is = calcIntersection r os
  if is == Nothing
    then return []
    else do
      let (p, n, m) = fromJust is
      i <- russianRoulette wl [reflectance m]
      pcs <- if i > 0
        then reflect p n os l wl
        --then return []
        else return []
      --if l > 1 && diffuseness m > 0.0
      if diffuseness m > 0.0
        then return $ ((wl, initRay p (getDir r)) : pcs)
        else return pcs

reflect :: Position3 -> Direction3 -> [Object] -> Int -> Wavelength
        -> IO [PhotonCache]
reflect p n os l wl = do
  dr <- diffuseReflection n
  let r' = initRay p dr
  pcs <- tracePhoton os (l+1) (wl, r')
  return pcs

-----
-- RAY TRACING WITH PHOTON MAP
-----

sr_half :: Double
sr_half = 1.0 / (2.0 * pi)

traceRay :: Int -> Double -> KdTree Double PhotonInfo -> [Object] -> Ray
         -> IO Radiance
traceRay 10 _ _ _ _ = return radiance0
traceRay l pw pmap objs r
  | is == Nothing = return radiance0
  | otherwise     = return (sr_half *> emittance m
                  + estimateRadiance pw pmap (p, n, m))
  where
    is = calcIntersection r objs
    (p, n, m) = fromJust is

estimateRadiance :: Double -> KdTree Double PhotonInfo -> Intersection
                 -> Radiance
estimateRadiance pw pmap (p, n, m)
  | ps == []  = radiance0
  | otherwise = (1.0 / (pi * rmax * rmax)) *> (brdf m rad)
  where
    ps = filter (isValidPhoton p n) $ kNearest pmap nPhoton $ photonDummy p
    rs = map (\x -> norm ((photonPos x) - p)) ps
    rmax = maximum rs
    rad = sumRadiance1 n pw rmax rs ps

isValidPhoton :: Position3 -> Direction3 -> PhotonInfo -> Bool
--isValidPhoton p n ph = True
isValidPhoton p n ph = square (p - photonPos ph) < radius2

-- filtering:
--   sumRadiance1  non filter
--   sumRadiance2  cone filter
--   sumRadiance3  gauss filter

-- Normal (non filter)
sumRadiance1 :: Direction3 -> Double -> Double -> [Double] -> [PhotonInfo]
             -> Radiance
sumRadiance1 n pw rmax rs ps = foldl (+) radiance0 rads
  where
    rads = map (photonInfoToRadiance n pw) ps

-- Cone filter
k_cone :: Double
k_cone = 1.1

fac_k :: Double
fac_k = 1.0 - 2.0 / (3.0 * k_cone)

sumRadiance2 :: Direction3 -> Double -> Double -> [Double] -> [PhotonInfo]
             -> Radiance
sumRadiance2 n pw rmax rs ps = foldl (+) radiance0 rads
  where
    wt = map (waitCone (pw / fac_k) rmax) rs
    rads = zipWith (photonInfoToRadiance n) wt ps

waitCone :: Double -> Double -> Double -> Double
waitCone pw rmax dp = pw * (1.0 - dp / (k_cone * rmax))

-- Gauss filter

alpha :: Double
alpha = 0.918

beta :: Double
beta  = 1.953

e_beta :: Double
e_beta = 1.0 - exp (-beta)

sumRadiance3 :: Direction3 -> Double -> Double -> [Double] -> [PhotonInfo]
             -> Radiance
sumRadiance3 n pw rmax rs ps = foldl (+) radiance0 rads
  where
    wt = map (waitGauss pw rmax) rs
    rads = zipWith (photonInfoToRadiance n) wt ps

waitGauss :: Double -> Double -> Double -> Double
waitGauss pw rmax dp = pw * alpha * (1.0 - e_r / e_beta)
  where
    e_r = 1.0 - exp (-beta * dp * dp / (2.0 * rmax * rmax))

------
-- CLASICAL RAY TRACING
------

amb :: Radiance
--amb = Radiance 0.002 0.002 0.002
amb = Radiance 0.00 0.00 0.00

traceRay' :: Int -> [Light] -> [Object] -> Ray -> Radiance
traceRay' l lgts objs r
  | is == Nothing = radiance0
  | otherwise     = sr_half *> emittance m + brdf m (radDiff + amb)
  where
    is = calcIntersection r objs
    (p, n, m) = fromJust is
    radDiff = foldl (+) radiance0 $ map (getRadianceFromLight objs p n) lgts

getRadianceFromLight :: [Object] -> Position3 -> Direction3 -> Light
                     -> Radiance
getRadianceFromLight objs p n l = sum $ zipWith (*>) coss $ getRadiance l dists
  where
    (dists, coss) = unzip $ illuminated objs p n $ getDirection l p

illuminated :: [Object] -> Position3 -> Direction3 -> [Direction3]
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
    (p', _, _) = fromJust is
    sqLdist = square ld
    sqOdist = square (p' - p)

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
brdf m rad = (diffuseness m / pi2) *> ((reflectance m) <**> rad)
--brdf m rad = (1.0 / pi2) *> ((reflectance m) <**> rad)

readMap :: IO (Double, KdTree Double PhotonInfo)
readMap = do
  np' <- getLine
  pw' <- getLine
  ps <- getContents
  let np = read np' :: Int
      pw = read pw' :: Double
      pcs = map (\x -> read x :: PhotonCache) (lines ps)
      pmap = build infoToPointList (map convertToInfo pcs)
  return (pw, pmap)
