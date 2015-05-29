{-# LANGUAGE NoImplicitPrelude #-}

--
-- Tracer
--

module Tracer where

import Data.Maybe
import Data.List
import Data.Ord
--import Data.Trees.KdTree
import Data.KdTree.Static
import NumericPrelude
import Debug.Trace

import Ray.Algebra
import Ray.Geometry
import Ray.Object
import Ray.Material
import Ray.Physics
import Ray.Optics

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
traceRay l pw pm os r = trace ("len=" ++ show (length pis)) (estimateRadiance pw n p m pis)
  where
    (p, n, m) = calcIntersection r os
    pin = PhotonInfo Red p ex3
    pis = kNearest pm 500 pin
    --pis = nearNeighbors pm 0.3 pin
    
calcIntersection :: Ray -> [Object] -> (Position3, Direction3, Material)
calcIntersection r os = (p, fromJust (getNormal p s), m)
  where
    p = target t r
    iss = filter (\x -> fst x > nearly0) (concat $ map (calcDistance r) os)
    (t, (Object s m)) = head $ sortBy (comparing fst) iss
    
generateRay :: Position3 -> Position3 -> (Double, Double)
            -> (Direction3, Direction3) -> (Int, Int) -> Ray
generateRay e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    target = o + ((sx * fromIntegral x) *> ex) + ((sy * fromIntegral y) *> ey)
    edir = target - e 
    edir' = fromJust $ normalize edir

