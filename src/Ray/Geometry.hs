--
-- Geometry
--

module Geometry
  (
  ) where

import NumericPrelude
import Ray.Algebra

type Ray = (Position3, Direction3)

initRay :: Position3 -> Vector3 -> Maybe Ray
initRay p v
  | v == o3   = Nothing
  | otherwise = Just (p, v')
  where
    v' = normalize v

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
  intersect :: Ray -> a -> [Position3]

-- Point
data Point = Point Position3

instance Shape Point where
  getNormal p s = Nothing
  distance r s = []

-- Plain
data Plain = Plain Direction3 Double

instance Shape Plain where
  getNormal p (Plain n d) = Just n
  distance (pos, dir) (Plain n d)
    | cos == 0  = []
    | otherwise = [(d + n <.> pos) / (-cos)]
    where
      cos = n <.> dir

-- Sphere
data Sphere = Sphere Position3 Double

instance Shape Sphere where
  getNormal p (Sphere c r) = normalize (p - c)
  intersect (pos, dir) (Sphere c r)
    | t1 <= 0.0 = []
    | t2 == 0.0 = [t0]
    | t1 >  0.0 = [t0 - t2, t0 + t2]
    where
      o  = c - pos
      t0 = o <.> dir
      t1 = r * r - (square o - (t0 * t0))
      t2 = sqrt t1

