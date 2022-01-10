{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Geometry
--

module Ray.Geometry (
  Ray
, Shape (Point, Plain, Sphere, Parallelogram)
, distance
, getDir
, getNormal
, getPos
, initParallelogram
, initPolygon
, initRay
, initRayFromElem
, methodMoller
, one_pi
, pi4
, randomPoint
, sqpi2
, sr_half
, surfaceArea
, target
) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.Maybe
import GHC.Generics
import NumericPrelude
import System.Random.Mersenne as MT

import Ray.Algebra

-- CONSTANTS

pi4 :: Double
pi4 = 4 * pi           -- for decay by distance (1/ 4pi) 

sqpi2 :: Double
sqpi2 = 2 * pi * pi    -- pi x steradian (2pi) for half sphere

one_pi :: Double
one_pi = 1.0 / pi      -- one of pi (integral of hemisphere)

sr_half :: Double
sr_half = 1.0 / (2.0 * pi)  -- half of steradian


-- Ray

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

-- Shapes
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
  Polygon
  { position :: !Position3
  , nvec     :: !Direction3
  , dir1     :: !Direction3
  , dir2     :: !Direction3
  }
  |
  Parallelogram
  { position :: !Position3
  , nvec     :: !Direction3
  , dir1     :: !Direction3
  , dir2     :: !Direction3
  }
  deriving (Eq, Show, Generic)

instance NFData Shape where
  rnf = genericRnf


initPolygon :: Position3 -> Position3 -> Position3 -> Shape
initPolygon p0 p1 p2 = Polygon p0 nvec d1 d2
  where
    d1 = p1 - p0
    d2 = p2 - p0
    nvec  = fromJust (normalize (d1 <*> d2))

initParallelogram :: Position3 -> Position3 -> Position3 -> Shape
initParallelogram p0 p1 p2 = Parallelogram p0 nvec d1 d2
  where
    d1 = p1 - p0
    d2 = p2 - p0
    nvec  = fromJust (normalize (d1 <*> d2))

getNormal :: Position3 -> Shape -> Maybe Direction3
-- Plain
getNormal _ (Plain nvec _) = Just nvec
-- Sphere
getNormal pos (Sphere center _) = normalize (pos - center)
-- Polygon
getNormal _ (Polygon _ nvec _ _) = Just nvec
-- Parallelogram
getNormal _ (Parallelogram _ nvec _ _) = Just nvec
-- Point
getNormal _ _ = Nothing


distance :: Ray -> Shape -> [Double]
-- Plain
distance (pos, dir) (Plain nvec d)
  | cos == 0  = []
  | otherwise = [(d + nvec <.> pos) / (-cos)]
  where
    cos = nvec <.> dir
-- Sphere
distance (pos, dir) (Sphere center radius)
  | t1 <= 0.0 = []
  | t2 == 0.0 = [t0]
  | t1 >  0.0 = [t0 - t2, t0 + t2]
  where
    o  = center - pos
    t0 = o <.> dir
    t1 = radius * radius - (square o - (t0 * t0))
    t2 = sqrt t1
-- Polygon
distance (pos, dir) (Polygon pos0 _ dir1 dir2)
  | res == Nothing = []
  | otherwise      = [t]
  where
    res = methodMoller 1.0 pos0 dir1 dir2 pos dir
    (_, _, t) = fromJust res
-- Parallelogram
distance (pos, dir) (Parallelogram pos0 _ dir1 dir2)
  | res == Nothing = []
  | otherwise      = [t]
  where
    res = methodMoller 2.0 pos0 dir1 dir2 pos dir
    (_, _, t) = fromJust res
-- Point
distance _ _ = []

{- |
surfaceArea 表面積
-}

surfaceArea :: Shape -> Double
surfaceArea (Point _) = 0.0
surfaceArea (Plain _ _) = 0.0
surfaceArea (Sphere _ radius) = 4 * pi * radius * radius
surfaceArea (Polygon _ _ dir1 dir2) = norm (dir1 <*> dir2) / 2.0
surfaceArea (Parallelogram _ _ dir1 dir2) = norm (dir1 <*> dir2)

{-
randomPoint 表面上のランダムな点
-}

randomPoint :: Shape -> IO Position3
randomPoint (Point pos) = return pos
randomPoint (Plain _ _) = return o3
randomPoint (Sphere center radius) = do
  dir <- generateRandomDir3
  return (center + (radius *> dir))
randomPoint (Polygon pos _ dir1 dir2) = do
  m <- MT.randomIO :: IO Double
  n <- MT.randomIO :: IO Double
  let
    (m', n') = if m + n > 1.0
      then if m > n
        then ((1.0 - m), n)
        else (m, (1.0 - n))
      else (m, n)
  return (pos + m' *> dir1 + n' *> dir2)
randomPoint (Parallelogram pos _ dir1 dir2) = do
  m <- MT.randomIO :: IO Double
  n <- MT.randomIO :: IO Double
  return (pos + m *> dir1 + n *> dir2)

--
-- UTILS
--

-- ポリゴンの当たり判定
-- https://shikousakugo.wordpress.com/2012/07/01/ray-intersection-3/
methodMoller :: Double -> Position3 -> Direction3 -> Direction3
             -> Position3 -> Direction3
             -> Maybe (Double, Double, Double)
methodMoller l pos0 dir1 dir2 pos dir
  | detA == 0.0        = Nothing
  | u < 0.0 || u > 1.0 = Nothing
  | v < 0.0 || v > 1.0 = Nothing
  | u + v > l          = Nothing
  | otherwise          = Just (u, v, t)
  where
    re2 = dir <*> dir2
    detA = re2 <.> dir1
    p'   = pos - pos0
    te1  = p' <*> dir1
    u    = (re2 <.> p') / detA
    v    = (te1 <.> dir)  / detA
    t    = (te1 <.> dir2) / detA
