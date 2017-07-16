{-# LANGUAGE NoImplicitPrelude #-}

--
-- Geometry
--

module Ray.Geometry (
  Ray
, Shape (Point, Plain, Sphere, Parallelogram)
, distance
, getDir
, getPos
, getNormal
, initRay
, initRayFromElem
, target
, diffuseReflection
, methodMoller
) where

import Data.Maybe
import NumericPrelude

import Ray.Algebra

type Ray = (Position3, Direction3)

initRay :: Position3 -> Direction3 -> Ray
initRay pos dir = (pos, dir)

initRayFromElem :: Double -> Double -> Double -> Double -> Double -> Double
                -> Maybe Ray
initRayFromElem px py pz dx dy dz
  | dir == Nothing = Nothing
  | otherwise      = Just (pos, fromJust dir)
  where
    pos = initPos px py pz
    dir = initDir dx dy dz

target :: Double -> Ray -> Position3
target t (pos, dir) = pos + t *> dir

getPos :: Ray -> Position3
getPos = fst

getDir :: Ray -> Direction3
getDir = snd

-- Shape
-----------------------

data Shape =
  Point
  { position :: !Position3
  }
  |
  Plain
  { nvec   :: !Direction3
  , dist   :: !Double
  }
  |
  Sphere
  { center :: !Position3
  , radius :: !Double
  }
  |
  Parallelogram
  { position :: !Position3
  , nvec     :: !Direction3
  , dir1     :: !Direction3
  , dir2     :: !Direction3
  }
  deriving Eq

getNormal :: Position3 -> Shape -> Maybe Direction3
-- Plain
getNormal _ (Plain n _) = Just n
-- Sphere
getNormal p (Sphere c _) = normalize (p - c)
-- Parallelogram
getNormal _ (Parallelogram _ n _ _) = Just n 
-- Point
getNormal _ _ = Nothing


distance :: Ray -> Shape -> [Double]
-- Plain
distance (pos, dir) (Plain n d)
  | cos0 == 0 = []
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
-- Parallelogram
distance (pos, dir) (Parallelogram p _ d1 d2)
  | res == Nothing = []
  | otherwise      = [t]
  where
    res = methodMoller 2.0 p d1 d2 pos dir
    (_, _, t) = fromJust res
-- Point
distance _ _ = []

--
-- REFLECTION AND REFRACTION
--

diffuseReflection :: Direction3 -> IO Direction3
diffuseReflection n = do
  dir <- generateRandomDir4
  let cos = n <.> dir
  return $ if cos > 0.0 then dir else negate dir

--
-- UTILS
--

-- ポリゴンの当たり判定
-- https://shikousakugo.wordpress.com/2012/07/01/ray-intersection-3/
methodMoller :: Double -> Position3 -> Direction3 -> Direction3
             -> Position3 -> Direction3
             -> Maybe (Double, Double, Double)
methodMoller l p0 d1 d2 p d
  | detA == 0.0        = Nothing
  | u < 0.0 || u > 1.0 = Nothing
  | v < 0.0 || v > 1.0 = Nothing
  | u + v > l          = Nothing
  | otherwise          = Just (u, v, t)
  where
    re2 = d <*> d2
    detA = re2 <.> d1
    p'   = p - p0
    te1  = p' <*> d1
    u    = (re2 <.> p') / detA
    v    = (te1 <.> d)  / detA
    t    = (te1 <.> d2) / detA
