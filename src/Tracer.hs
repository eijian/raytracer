{-# LANGUAGE NoImplicitPrelude #-}
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

import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.List hiding (sum)
import           Data.Ord
--import qualified Data.KdTree.Static as KT
--import qualified Data.KdTree.Dynamic as KT
--import           Debug.Trace
import           NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Object
import Ray.Light
import Ray.Material
import Ray.Physics
import Ray.Optics

import Screen
import Scene

--
-- CONSTANTS
--

--sqpi2 :: Double
--sqpi2 = 2 * pi * pi    -- pi x steradian (2pi) for half sphere

one_pi :: Double
one_pi = 1.0 / pi      -- one of pi (integral of hemisphere)

sr_half :: Double
sr_half = 1.0 / (2.0 * pi)  -- half of steradian

--

tracePhoton :: Bool -> Material -> [Object] -> Int -> Photon
            -> IO [PhotonCache]
tracePhoton _   _   _   10 _        = return []
tracePhoton !uc !m0 !os !l !(wl, r)
  | is == Nothing = return []
  | otherwise     = do
    let
      is' = fromJust is
      (p, _, m) = is'
      d = diffuseness m
    i <- russianRoulette [d]
    ref <- if i > 0
      then reflectDiff uc m0 os l wl is'
      else reflectSpec uc m0 os l (wl, r) is'
    if (uc == False || l > 0) && d > 0.0
      then return $ ((wl, initRay p $ getDir r) : ref)
      else return ref
  where
    is = calcIntersection r os

reflectDiff :: Bool -> Material -> [Object] -> Int -> Wavelength
            -> Intersection -> IO [PhotonCache]
reflectDiff uc m0 os l wl (p, n, m) = do
  i <- russianRoulette [selectWavelength wl $ reflectance m]
  if i > 0
    then do  -- diffuse reflection
      dr <- diffuseReflection n
      tracePhoton uc m0 os (l+1) $ (wl, initRay p dr)
    else return [] -- absorption

reflectSpec :: Bool -> Material -> [Object] -> Int -> Photon -> Intersection
            -> IO [PhotonCache]
reflectSpec uc m0 os l (wl, (_, ed)) (p, n, m) = do
  let
    f0 = selectWavelength wl $ specularRefl m
    (rdir, cos0) = specularReflection n ed
    f' = f0 + (1.0 - f0) * (1.0 - cos0) ** 5.0
  j <- russianRoulette [f']
  if j > 0
    then tracePhoton uc m0 os (l+1) (wl, initRay p rdir)
    else do
      if (selectWavelength wl $ ior m) == 0.0
        then return []   -- non transparency
        else reflectTrans uc m0 os l wl ed (p, n, m) cos0

reflectTrans :: Bool -> Material -> [Object] -> Int -> Wavelength -> Direction3
             -> Intersection -> Double -> IO [PhotonCache]
reflectTrans uc m0 os l wl ed (p, n, m) c0 = do
  let
    ior0 = selectWavelength wl $ ior m0
    ior1 = selectWavelength wl $ ior m
    (tdir, ior') = specularRefraction ior0 ior1 c0 ed n
    m0' = if tdir <.> n < 0.0 then m else m_air
  tracePhoton uc m0' os (l+1) (wl, initRay p tdir)

-----
-- RAY TRACING WITH PHOTON MAP
-----

traceRay :: Screen -> Material -> Int -> PhotonMap -> [Object] -> [Light]
         -> Ray -> IO Radiance
traceRay _    _   10 _     _     _     _  = return radiance0
traceRay !scr !m0 !l !pmap !objs !lgts !r
  | is == Nothing = return radiance0
  | otherwise     = do
    si <- if df == 1.0 || f == black
      then return radiance0
      else traceRay scr m0 (l+1) pmap objs lgts (initRay p rdir)
    ti <- if f' == black || ior1 == 0.0
      then return radiance0
      else do
        let
          ior0 = averageIor m0
          (tdir, ior') = specularRefraction ior0 ior1 cos0 (getDir r) n
          m0' = if tdir <.> n < 0.0 then m else m_air
        traceRay scr m0' (l+1) pmap objs lgts (initRay p tdir)
    return (sr_half    *> emittance m +
            df         *> brdf m (di + ii) +
            (1.0 - df) *> (f <**> si + (1.0 - mt) *> f' <**> ti))
  where
    is = calcIntersection r objs
    (p, n, m) = fromJust is
    di = if useClassicForDirect scr
      then foldl (+) radiance0 $ map (getRadianceFromLight objs p n) lgts
      else radiance0
    ii = estimateRadiance scr pmap (p, n, m)
    (rdir, cos0) = specularReflection n (getDir r)
    df = diffuseness m
    mt = metalness m
    f = reflectionIndex (specularRefl m) cos0
    f' = negateColor f                         -- this means '1 - f'
    ior1 = averageIor m

estimateRadiance :: Screen -> PhotonMap -> Intersection -> Radiance
estimateRadiance scr pmap (p, n, m)
  | ps == []  = radiance0
  | otherwise = (1.0 / (pi * rmax * rmax)) *> rad
  where
    ps = filter adopt $ (nearest pmap) $ photonDummy p
    rs = map (\x -> norm ((photonPos x) - p)) ps
    rmax = maximum rs
    sumfunc = case (pfilter scr) of
                Nonfilter   -> sumRadiance1
                Conefilter  -> sumRadiance2
                Gaussfilter -> sumRadiance3
    rad = sumfunc n (power pmap) rmax rs ps
    adopt :: PhotonInfo -> Bool
    adopt ph
      | radius2 scr == 0.0 = True
      | otherwise          = square (p - photonPos ph) < (radius2 scr)

-- filtering:
--   sumRadiance1  non filter
--   sumRadiance2  cone filter
--   sumRadiance3  gauss filter

-- Normal (non filter)
sumRadiance1 :: Direction3 -> Double -> Double -> [Double] -> [PhotonInfo]
             -> Radiance
sumRadiance1 n pw _ _ ps = foldl (+) radiance0 rads
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
    waitGauss p r dp = p * alpha * (1.0 - e_r / e_beta)
      where
        e_r = 1.0 - exp (-beta * dp * dp / (2.0 * r * r))

------
-- CLASICAL RAY TRACING
------

traceRay' :: Screen -> Int -> [Light] -> [Object] -> Ray -> IO Radiance
traceRay' !scr l lgts objs r
  | is == Nothing = return radiance0
  | otherwise     = return (
                    sr_half *> emittance m
                  + brdf m (radDiff + (ambient scr))
                  )
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
  | nvec == Nothing = Nothing
  | otherwise = Just (p, fromJust nvec, m)
  where
    p = target t r
    iss = filter (\x -> fst x > nearly0) (concat $ map (calcDistance r) os)
    (t, (Object s m)) = head $ sortBy (comparing fst) iss
    nvec = getNormal p s

calcDistance :: Ray -> Object -> [(Double, Object)]
calcDistance r o@(Object s _) = zip ts (replicate (length ts) o)
  where
    ts = distance r s

brdf :: Material -> Radiance -> Radiance
--brdf m rad = (diffuseness m / pi2) *> ((reflectance m) <**> rad)
brdf m rad = one_pi *> ((reflectance m) <**> rad)

