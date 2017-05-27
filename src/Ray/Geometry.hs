{-# LANGUAGE NoImplicitPrelude #-}

--
-- Geometry
--

module Ray.Geometry (
  Ray
, Shape (Point, Plain, Sphere)
, distance
, getDir
, getPos
, getNormal
, initRay
, initRayFromElem
, target
, diffuseReflection
) where

import Data.Maybe
import NumericPrelude

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

data Shape = Point Position3
           | Plain Direction3 Double
           | Sphere Position3 Double
           deriving Eq

getNormal :: Position3 -> Shape -> Maybe Direction3
-- Plain
getNormal _ (Plain n _) = Just n
-- Sphere
getNormal p (Sphere c _) = normalize (p - c)
-- Point
getNormal _ _ = Nothing

distance :: Ray -> Shape -> [Double]
-- Plain
distance (pos, dir) (Plain n d)
  | cos0 == 0  = []
  | otherwise = [(d + n <.> pos) / (-cos0)]
  where
    cos0 = n <.> dir
-- Sphere
distance (pos, dir) (Sphere c r)
  | t1 <= 0.0 = []
  | t2 == 0.0 = [t0]
  | t1 >  0.0 = [t0 - t2, t0 + t2]
  where
    o  = c - pos
    t0 = o <.> dir
    t1 = r * r - (square o - (t0 * t0))
    t2 = sqrt t1
-- Point
distance _ _ = []

--
-- REFLECTION AND REFRACTION
--

diffuseReflection :: Direction3 -> IO Direction3
diffuseReflection n = do
  d <- generateRandomDir4
  let cos = n <.> d
  return $ if cos > 0.0 then d else negate d
