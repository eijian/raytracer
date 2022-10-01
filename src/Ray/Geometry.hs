{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Geometry
--

module Ray.Geometry (
  InOut (..)
, Ray
, Shape (Point, Plain, Sphere, Parallelogram, Mesh)
, SurfacePoint
, Vertex
, distance
, getDir
, getNormal
, getPos
, initParallelogram
, initParallelogramWithNormal
, initPolygon
, initPolygonWithNormal
, initRay
, initRayFromElem
, methodMoller
, nSurface
, one_pi
, pi2
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
import Debug.Trace
import GHC.Generics
import NumericPrelude
import System.Random.Mersenne as MT

import Ray.Algebra

-- CONSTANTS

pi2 :: Double
pi2 = 2 * pi

pi4 :: Double
pi4 = 4 * pi           -- for decay by distance (1/ 4pi) 

sqpi2 :: Double
sqpi2 = 2 * pi * pi    -- pi x steradian (2pi) for half sphere

one_pi :: Double
one_pi = 1.0 / pi      -- one of pi (integral of hemisphere)

sr_half :: Double
sr_half = 1.0 / (2.0 * pi)  -- half of steradian

-- common

data InOut = In | Out
  deriving (Eq, Show, Generic)

instance NFData InOut where
  rnf = genericRnf

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

-- SurfacePoint

type SurfacePoint = (Position3, Direction3)



-- Shapes
-----------------------

type Vertex = (Int, Int, Int)   -- 頂点型：頂点座標番号＋法線ベクトル番号＋マッピング座標番号
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
  , uvmaps   :: !(UA.Array Int Vector2)
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

initPolygonWithNormal :: Position3 -> Position3 -> Position3 -> Direction3
  -> Shape
initPolygonWithNormal p0 p1 p2 nvec = Polygon p0 nvec' d1 d2
  where
    d1 = p1 - p0
    d2 = p2 - p0
    nvec'  = fromJust $ normalize nvec

initParallelogram :: Position3 -> Position3 -> Position3 -> Shape
initParallelogram p0 p1 p2 = Parallelogram p0 nvec d1 d2
  where
    d1 = p1 - p0
    d2 = p2 - p0
    nvec  = fromJust (normalize (d1 <*> d2))

initParallelogramWithNormal :: Position3 -> Position3 -> Position3 -> Direction3
  -> Shape
initParallelogramWithNormal p0 p1 p2 nvec = Parallelogram p0 nvec' d1 d2
  where
    d1 = p1 - p0
    d2 = p2 - p0
    nvec'  = fromJust $ normalize nvec

getNormal :: Position3 -> Shape -> Maybe Direction3
-- Plain
getNormal _ (Plain nvec _) = Just nvec
-- Sphere
getNormal pos (Sphere center _) = normalize (pos - center)
-- Polygon
getNormal _ (Polygon _ nvec _ _) = Just nvec
-- Parallelogram
getNormal _ (Parallelogram _ nvec _ _) = Just nvec
-- Mesh
getNormal _ (Mesh ps _ _ _) = Nothing
-- Point
getNormal _ _ = Nothing


dammypatch :: Patch
dammypatch = ((0, 0, 0), (0, 0, 0), (0, 0, 0))

distance :: Ray -> Shape -> Maybe (Vector2, Double, Shape)
-- Plain
distance (pos, dir) shape@(Plain nvec d)
  | cos == 0       = Nothing
  | dist < nearly0 = Nothing
  | otherwise      = Just (o2, dist, shape)
  where
    cos = nvec <.> dir
    dist = (d + nvec <.> pos) / (-cos)
-- Sphere
distance (pos, dir) shape@(Sphere center radius)
  | t1 <= 0.0 = Nothing
  | t2 == 0.0 = if t0 >= nearly0
      then Just (o2, t0, shape)
      else Nothing
  | t1 >  0.0 = if t0 - t2 >= nearly0
      then Just (o2, t0 - t2, shape)
      else if t0 + t2 >= nearly0
        then Just (o2, t0 + t2, shape)
        else Nothing
  where
    o  = center - pos
    t0 = o <.> dir
    t1 = radius * radius - (square o - (t0 * t0))
    t2 = sqrt t1
-- Polygon
distance (pos, dir) shape@(Polygon pos0 _ dir1 dir2)
  | res == Nothing = Nothing
  | t < nearly0    = Nothing
  | otherwise      = Just (uv, t, shape)
  where
    res = methodMoller 1.0 pos0 dir1 dir2 pos dir
    (uv, t) = fromJust res
-- Parallelogram
distance (pos, dir) shape@(Parallelogram pos0 _ dir1 dir2)
  | res == Nothing = Nothing
  | t < nearly0    = Nothing
  | otherwise      = Just (uv, t, shape)
  where
    res = methodMoller 2.0 pos0 dir1 dir2 pos dir
    (uv, t) = fromJust res
-- Mesh
distance ray (Mesh ps vs ns _) = if t >= infinity
  then Nothing
  else if t >= nearly0
    then Just (uv, t, buildPoly p d1 d2)
    else Nothing
  where
    dampat = ((0, 0, 0), (0, 0, 0), (0, 0, 0))
    (uv, t, p, d1, d2) = foldl' (compPolygon ray vs) (o2, infinity, dammypatch, o3, o3) ps
    buildPoly :: Patch -> Direction3 -> Direction3 -> Shape
    buildPoly ((p0, n0, _), _, _) d1' d2' = Polygon (vs UA.! p0) (ns UA.! n0) d1' d2'

-- Point
distance _ _ = Nothing

{-
nSurface  面の数
-}

nSurface :: Shape -> Int
nSurface (Point _) = 0
nSurface (Plain _ _) = 1
nSurface (Sphere _ _) = 1
nSurface (Polygon _ _ _ _) = 1
nSurface (Parallelogram _ _ _ _) = 1
nSurface (Mesh ps _ _ _) = V.length ps

{- |
surfaceArea 表面積
-}

surfaceArea :: Shape -> Double
surfaceArea (Point _) = 0.0
surfaceArea (Plain _ _) = 0.0
surfaceArea (Sphere _ radius) = 4 * pi * radius * radius
surfaceArea (Polygon _ _ dir1 dir2) = norm (dir1 <*> dir2) / 2.0
surfaceArea (Parallelogram _ _ dir1 dir2) = norm (dir1 <*> dir2)
surfaceArea (Mesh ps vtxs _ _) = foldl' (sumPatchArea) 0.0 ps
  where
    sumPatchArea :: Double -> Patch -> Double
    sumPatchArea s ((p0, _, _), (p1, _, _), (p2, _, _)) = s + (norm (d1 <*> d2) / 2.0)
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
randomPoint (Mesh ps vtxs norms _) = do
  ri <- MT.randomIO :: IO Double
  let
    len = fromIntegral $ V.length ps
    ri' = len * ri
    i = if ri' == len then truncate (len - 1) else truncate ri'
    ((p0, nvec, _), (p1, _, _), (p2, _, _)) = ps V.! i
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
  return (vtxs UA.! p0 + m' *> d1 + n' *> d2, norms UA.! nvec)

getDirection :: Shape -> Position3 -> Position3 -> Maybe Direction3
getDirection shp destpos srcpos
  | nvec == Nothing = Nothing
  | cos > 0.0       = Nothing
  | otherwise       = Just dir
  where
    nvec = getNormal destpos shp
    dir = destpos - srcpos
    cos = (fromJust nvec) <.> dir


--
-- UTILS
--

-- ポリゴンの当たり判定
-- https://shikousakugo.wordpress.com/2012/07/01/ray-intersection-3/
methodMoller :: Double -> Position3 -> Direction3 -> Direction3
             -> Position3 -> Direction3
             -> Maybe (Vector2, Double)
methodMoller l pos0 dir1 dir2 pos dir
  | detA == 0.0        = Nothing
  | u < 0.0 || u > 1.0 = Nothing
  | v < 0.0 || v > 1.0 = Nothing
  | u + v > l          = Nothing
  | otherwise          = Just ((u, v), t)
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

compPolygon :: Ray -> UA.Array Int Position3
  -> (Vector2, Double, Patch, Direction3, Direction3) -> Patch
  -> (Vector2, Double, Patch, Direction3, Direction3)
compPolygon (pos, dir) vtxs d@(_, t, _, _, _) p@((p0, _, _), (p1, _, _), (p2, _, _)) =
  case res of
    Just (uv', t') -> if nearly0 <= t' && t' < t
      then (uv', t', p, d1, d2)
      else d
    Nothing        -> d
  where
    d1 = vtxs UA.! p1 - vtxs UA.! p0
    d2 = vtxs UA.! p2 - vtxs UA.! p0
    res = methodMoller 1 (vtxs UA.! p0) d1 d2 pos dir

infinity :: Double
infinity = 1e100
