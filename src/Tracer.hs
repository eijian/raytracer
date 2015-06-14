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

--
--

tracePhoton :: [Object] -> Photon -> IO [PhotonCache]
tracePhoton os (wl, r) = do
  let iss = filter (\x -> fst x > nearly0) (concat $ map (calcDistance r) os)
      (t, s) = head $ sortBy (comparing fst) iss
  return [(wl, initRay (target t r) (getDir r))]

calcDistance :: Ray -> Object -> [(Double, Object)]
calcDistance r o@(Object s m) = zip ts (replicate (length ts) o)
  where
    ts = distance r s

---

traceRay :: Int -> Double -> KdTree Double PhotonInfo -> [Object] -> Ray
         -> Radiance
traceRay 10 _ _ _ _ = Radiance 0 0 0
traceRay l pw pm os r
  | is == Nothing = Radiance 0 0 0
  | otherwise    = estimateRadiance pw n p m pis
  where
    is = calcIntersection r os
    (p, n, m) = fromJust is
    pin = PhotonInfo Red p ex3
    pis = kNearest pm nPhoton pin
    --pis = inRadius pm 0.2 pin

traceRay' :: Int -> [Light] -> [Object] -> Ray -> Radiance
traceRay' l lgts os r
  | is == Nothing = Radiance 0 0 0
  | otherwise     = getRadianceFromLights os lgts p n
  where
    is = calcIntersection r os
    (p, n, m) = fromJust is

getRadianceFromLights :: [Object] -> [Light] -> Position3 -> Direction3
                      -> Radiance
getRadianceFromLights os (l:ls) p n
  | is == Nothing = (cos' *> (getRadiance l p)) + (getRadianceFromLights os ls p n)
  | otherwise     = Radiance 0 0 0
  where
    ldir = getDirection l p
    lray = initRay p ldir
    is = calcIntersection lray os
    (p, n, m) = fromJust is
    cos = n <.> ldir
    cos' = if cos < 0 then 0 else cos
    
calcIntersection :: Ray -> [Object] -> Maybe (Position3, Direction3, Material)
calcIntersection r os
  | iss == [] = Nothing
  | otherwise = Just (p, fromJust (getNormal p s), m)
  where
    p = target t r
    iss = filter (\x -> fst x > nearly0) (concat $ map (calcDistance r) os)
    (t, (Object s m)) = head $ sortBy (comparing fst) iss
    
generateRay :: Position3 -> Position3 -> (Double, Double)
            -> (Direction3, Direction3) -> (Int, Int) -> Ray
generateRay e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    tgt   = o + ((sx * fromIntegral x) *> ex) + ((sy * fromIntegral y) *> ey)
    edir  = tgt - e 
    edir' = fromJust $ normalize edir
