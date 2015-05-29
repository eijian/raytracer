--
-- Tracer
--

module Tracer where

import Data.Maybe
import Data.List
import Data.Ord
import Data.Trees.KdTree

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

traceRay :: Int -> Double -> KdTree PhotonInfo -> [Object] -> Ray -> Radiance
traceRay 10 _ _ _ _ = Radiance 0 0 0
traceRay l pw pm os r = estimateRadiance pw n p m pis
  where
    (p, n, m) = calcIntersection r os
    pi = PhotonInfo Red p ex3
    pis = kNearestNeighbors pm 100 pi
    
calcIntersection :: Ray -> [Object] -> (Position3, Direction3, Material)
calcIntersection r os = (p, fromJust (getNormal p s), m)
  where
    p = target t r
    iss = filter (\x -> fst x > nearly0) (concat $ map (calcDistance r) os)
    (t, (Object s m)) = head $ sortBy (comparing fst) iss
    
generateRay :: (Int, Int) -> Ray
generateRay (y, x) =
  initRay eyepos (origin + ((stepx * fromIntegral x) *> eex) +
                           ((stepy * fromIntegral y) *> eey))

