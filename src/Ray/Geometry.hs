{-# LANGUAGE NoImplicitPrelude #-}
--
-- Geometry
--

module Ray.Geometry where


import Data.Maybe
import Ray.Algebra

type Ray = (Position3, Direction3)

initRay :: Position3 -> Direction3 -> Ray
initRay p d = (p, d)

initRayFromElem :: Double -> Double -> Double -> Double -> Double -> Double
                -> Maybe Ray
initRayFromElem px py pz dx dy dz
  | d == Nothing = Nothing
  | otherwise    = Just (p, fromJust d)
  where
    p = initPos px py pz
    d = initDir dx dy dz

target :: Double -> Ray -> Position3
target t (p, d) = p + t *> d

getPos :: Ray -> Position3
getPos = fst

getDir :: Ray -> Direction3
getDir = snd

-- Shape
-----------------------

class Shape a where
  getNormal :: Position3 -> a -> Maybe Direction3
  distance :: Ray -> a -> [(Double, a)]

-- Point
data Point = Point Position3

instance Shape Point where
  getNormal p s = Nothing
  distance r s = []

-- Plain
data Plain = Plain Direction3 Double

instance Shape Plain where
  getNormal p (Plain n d) = Just n
  distance (pos, dir) s@(Plain n d)
    | cos == 0  = []
    | otherwise = [((d + n <.> pos) / (-cos), s)]
    where
      cos = n <.> dir

-- Sphere
data Sphere = Sphere Position3 Double

instance Shape Sphere where
  getNormal p (Sphere c r) = normalize (p - c)
  distance (pos, dir) s@(Sphere c r)
    | t1 <= 0.0 = []
    | t2 == 0.0 = [(t0, s)]
    | t1 >  0.0 = [(t0 - t2, s), (t0 + t2, s)]
    where
      o  = c - pos
      t0 = o <.> dir
      t1 = r * r - (square o - (t0 * t0))
      t2 = sqrt t1

