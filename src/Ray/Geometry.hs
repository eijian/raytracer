{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Geometry
--

module Ray.Geometry (
  Ray
, Shape (Point, Plain, Sphere, Parallelogram, Mesh)
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
import Data.Array.Unboxed as UA
import Data.Int
import Data.Maybe
import Data.Vector as V
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

type Vertex = (Int, Int)   -- 頂点型：頂点座標の番号＋法線ベクトルの番号
type Patch = (Vertex, Vertex, Vertex)

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
  |
  Mesh
  { patches  :: !(V.Vector Patch)
  , vertexes :: !(UA.Array Int Position3)
  , normals  :: !(UA.Array Int Direction3)
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
-- Mesh
getNormal _ (Mesh ps _ _) = Nothing

distance :: Ray -> Shape -> [(Double, Shape)]
-- Plain
distance (pos, dir) shape@(Plain nvec d)
  | cos == 0  = []
  | otherwise = [((d + nvec <.> pos) / (-cos), shape)]
  where
    cos = nvec <.> dir
-- Sphere
distance (pos, dir) shape@(Sphere center radius)
  | t1 <= 0.0 = []
  | t2 == 0.0 = [(t0, shape)]
  | t1 >  0.0 = [(t0 - t2, shape), (t0 + t2, shape)]
  where
    o  = center - pos
    t0 = o <.> dir
    t1 = radius * radius - (square o - (t0 * t0))
    t2 = sqrt t1
-- Polygon
distance (pos, dir) shape@(Polygon pos0 _ dir1 dir2)
  | res == Nothing = []
  | otherwise      = [(t, shape)]
  where
    res = methodMoller 1.0 pos0 dir1 dir2 pos dir
    (_, _, t) = fromJust res
-- Parallelogram
distance (pos, dir) shape@(Parallelogram pos0 _ dir1 dir2)
  | res == Nothing = []
  | otherwise      = [(t, shape)]
  where
    res = methodMoller 2.0 pos0 dir1 dir2 pos dir
    (_, _, t) = fromJust res
-- Mesh
distance ray (Mesh ps vs ns) = if t >= infinity
  then []
  else [d]
  where
    d@(t, shape) = foldl' (compPolygon ray vs ns) (infinity, Point o3) ps
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
surfaceArea (Mesh ps vtxs _) = foldl' (sumPatchArea) 0.0 ps
  where
    sumPatchArea :: Double -> Patch -> Double
    sumPatchArea s ((p0, _), (p1, _), (p2, _)) = s + (norm (d1 <*> d2) / 2.0)
      where
        d1 = vtxs UA.! p1 - vtxs UA.! p0
        d2 = vtxs UA.! p2 - vtxs UA.! p0

{-
randomPoint 表面上のランダムな点
-}

randomPoint :: Shape -> IO (Position3, Direction3)
randomPoint (Point pos) = return (pos, o3)
randomPoint (Plain _ _) = return (o3, o3)
randomPoint shape@(Sphere center radius) = do
  dir <- generateRandomDir3
  let
    pos = center + (radius *> dir)
    nvec = getNormal pos shape
  return (pos, fromJust nvec)
randomPoint (Polygon pos nvec dir1 dir2) = do
  m <- MT.randomIO :: IO Double
  n <- MT.randomIO :: IO Double
  let
    (m', n') = if m + n > 1.0
      then if m > n
        then ((1.0 - m), n)
        else (m, (1.0 - n))
      else (m, n)
  return (pos + m' *> dir1 + n' *> dir2, nvec)
randomPoint (Parallelogram pos nvec dir1 dir2) = do
  m <- MT.randomIO :: IO Double
  n <- MT.randomIO :: IO Double
  return (pos + m *> dir1 + n *> dir2, nvec)
randomPoint (Mesh ps vtxs _) = do
  ri <- MT.randomIO :: IO Double
  let
    len = fromIntegral $ V.length ps
    ri' = len * ri
    i = if ri' == len then len - 1 else ri'
    ((p0, _), (p1, _), (p2, _)) = ps V.! (truncate i)
    d1 = vtxs UA.! p1 - vtxs UA.! p0
    d2 = vtxs UA.! p2 - vtxs UA.! p0
  m  <- MT.randomIO :: IO Double
  n  <- MT.randomIO :: IO Double
  let
    (m', n') = if m + n > 1.0
      then if m > n
        then ((1.0 - m), n)
        else (m, (1.0 - n))
      else (m, n)
  return (vtxs UA.! p0 + m' *> d1 + n' *> d2, d1 <*> d2)

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

--
-- PRIVATE
--

compPolygon :: Ray -> UA.Array Int Position3 -> UA.Array Int Position3 -> (Double, Shape) -> Patch
  -> (Double, Shape)
compPolygon (pos, dir) vtxs norms d@(t, shape) ((p0, n0), (p1, n1), (p2, n2)) =
  case res of
    Just (u, v, t') -> if t' < t
      then (t', Polygon (vtxs UA.! p0) (fromJust (normalize (d1 <*> d2))) d1 d2)
      -- initPolygon (vtxs UA.! p0) (vtxs UA.! p1) (vtxs UA.! p2))
      else d
    Nothing        -> d
  where
    d1 = vtxs UA.! p1 - vtxs UA.! p0
    d2 = vtxs UA.! p2 - vtxs UA.! p0
    res = methodMoller 1 (vtxs UA.! p0) d1 d2 pos dir

infinity :: Double
infinity = 1e100
