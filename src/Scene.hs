{-# LANGUAGE NoImplicitPrelude #-}

--
-- Scene
--

module Scene (
  m_air
, readScene
--, lgts
--, objs
) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Physics
import Ray.Optics
import Ray.Object
import Ray.Material

import Parser

--
-- CONSTANTS
--

m_air :: Material
m_air = Material radiance0 white white black (Color 1.0 1.0 1.0) 0.0 0.0 0.0

--
-- PUBLIC
--

readScene :: String -> IO ([Light], [Object])
readScene file = do
--  return (lgts, objs)
  lines <- readConfig file
  parseConfig ((intercalate "\n" lines) ++ "\n")

--
-- PRIVATE
--

readConfig :: String -> IO [String]
readConfig file = do
  f <- readFile file
  return $ map removeComment $ lines f

parseConfig :: String -> IO ([Light], [Object])
parseConfig conf = do
  let
    (ls, ms) = case (parse scene "rt scene file parse error" conf) of
      Left e -> error (show e)
      Right (l', m') -> (l', m')
    mmap = M.fromList ms
    shps = [
        (Plain ey3 0, "mwall") -- bottom
      , (Plain (negate ey3) 4, "mwall")
      , (Plain (negate ex3) 2, "mwallb")
      , (Plain ex3 2, "mwallr")
      , (Plain ez3 6, "mwall")
      , (Plain (negate ez3) 5, "mwall")
--      , (Sphere (initPos 0 0.8 3.0) 0.8, "mball")
--      , (Sphere (initPos 1 0.8 2.0) 0.8, "mball")
      , (Sphere (initPos (-1) 0.8 1.5) 0.8, "mball2")
      , (Sphere (initPos 1 0.8 2.5) 0.8, "mball")
--      , (initParallelogram bp1 bp2 bp4, "mball")
--      , (initParallelogram bp1 bp4 bp5, "mball")
--      , (initParallelogram bp4 bp3 bp8, "mball")
--      , (initParallelogram bp2 bp6 bp3, "mball")
--      , (initParallelogram bp7 bp6 bp8, "mball")
--      , (initParallelogram bp1 bp5 bp2, "mball")
--      , (initPolygon (tetra!!0) (tetra!!3) (tetra!!1), "mball")
--      , (initPolygon (tetra!!0) (tetra!!2) (tetra!!3), "mball")
--      , (initPolygon (tetra!!0) (tetra!!1) (tetra!!2), "mball")
--      , (initPolygon (tetra!!3) (tetra!!2) (tetra!!1), "m_ball")
--      , (Parallelogram (initPos (-0.5) 3.99 2.5) (negate ey3) ex3 ez3, "mparal")
      , (Parallelogram (initPos (-1.99) 3.99 3.99) (negate ey3) ez3 ex3, "msunl")
      ]
    (shapes, mapnames) = unzip shps
    os = zipWith initObject shapes $ map ((M.!) mmap) mapnames
  --return (lgts, os)
  return (ls, os)

{-
--pl1 :: Light
--pl2 :: Light
lgts :: [Light]
lgts = [pl1]
--lgts = [pl1, pl2]
  where
    --pl1 = PointLight (initColor 1 1 1) 5.0 (initPos 0 3.99 3.0) -- 5W
    pl1 = ParallelogramLight (initColor 1 1 1) 5.0 (initPos (-0.5) 3.99 2.5) (negate ey3) ex3 ez3  -- 5W Parallelogram
    --pl1 = SunLight (initColor 1 1 1) 20.0 (initPos (-1.99) 3.99 3.99) (negate ey3) ez3 ex3 (fromJust $ normalize $ Vector3 1 (-1) 1)  -- 10W Sun
    --pl1 = PointLight (initColor 1 1 1) 1.0 (initPos (-1.5) 3.5 4.5) -- 1W
    --pl2 = PointLight (initColor 2 1 0) 1.0 (initPos 1.5 3.5 4.5) -- 1W
-}

bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8 :: Position3
bp1 = Vector3 (-1.2) 1.8 2.6
bp2 = Vector3 (-0.4) 1.8 4.2
bp3 = Vector3 1.2    1.8 3.4
bp4 = Vector3 0.4    1.8 1.8
bp5 = Vector3 (-1.2) 0.2 2.6
bp6 = Vector3 (-0.4) 0.2 4.2
bp7 = Vector3 1.2    0.2 3.4
bp8 = Vector3 0.4    0.2 1.8

tetra :: [Position3]
--tetra = [Vector3 1.0 0.2 1.933, Vector3 1.92376 0.2 3.533333, Vector3 (1-0.92376) 0.2 3.5333333, Vector3 1 1.7 3]
--tetra = [Vector3 0.0 0.2 1.933, Vector3 0.92376 0.2 3.533333, Vector3 (-0.92376) 0.2 3.5333333, Vector3 0 1.7 3]
tetra = [Vector3 (-1) 0.2 2, Vector3 1 2.2 2, Vector3 (-1) 2.2 4, Vector3 1 0.2 4]

{-
objs :: [Object]
--objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball1, paralgt]
--objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, box_u, box_b, box_f, box_r, box_i, box_l, paralgt]
--objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, box_u, box_b, box_f, box_r, box_i, box_l, ball2, paralgt]
--objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball2, ball3]
objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball2, ball3, paralgt]
--objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, tetra1, tetra2, tetra3, tetra4, paralgt]
--objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball1]
  where
    --m_ball, m_wall, m_ceil, m_flor :: Material
    --m_ball = Material radiance0 (Color 0.5 0.2 0.2)  black black black 1.0 0.0 0.0
    --m_ball = Material radiance0 (Color 0.5 0.5 0.5)  black black black 1.0 0.0 0.0
    --m_ball = Material radiance0 (Color 0.5 0.5 0.5)  black (Color 0.2 0.2 0.2) black 0.5 1.0 0.0
    --m_ball = Material radiance0 (Color 0.0 0.0 0.0)  black (Color 1.0 0.71 0.29) black 0.0 1.0 0.0
    --m_ball = Material radiance0 (Color 0.0 0.0 0.0)  black (Color 0.78 0.78 0.78) black 0.0 1.0 0.0
    m_ball2 = Material radiance0 (Color 0.0 0.0 0.0)  black (Color 0.78 0.78 0.78) black 0.0 1.0 0.0
    m_ball = Material radiance0 (Color 0.0 0.0 0.0)  black (Color 0.08 0.08 0.08) (Color 1.5 1.5 1.5) 0.0 0.0 0.0
    m_wall = Material radiance0 (Color 0.5 0.5 0.5)  black black black 1.0 0.0 0.0
    --m_ceil = Material radiance0 (Color 0.4 0.2 0.02) black black black 1.0 0.0 0.0
    --m_flor = Material radiance0 (Color 0.5 0.3 0.1)  black black black 1.0 0.0 0.0
    m_wallr = Material radiance0 (Color 0.4 0.0 0.0)  black black black 1.0 0.0 0.0
    m_wallb = Material radiance0 (Color 0.0 0.0 0.4)  black black black 1.0 0.0 0.0
    pw = 5.0 / 1.0 / (2.0 * pi) :: Double
    m_paral = Material (Radiance pw pw pw) black black black black 0.0 0.0 0.0
    --m_sunl = Material (Radiance 0.01 0.015 0.02) black black black black 0.0 0.0 0.0

    --wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball1 :: Object
    --wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, box_u, box_b, box_f, box_r, box_i, box_l :: Object
    wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, tetra1, tetra2, tetra3, tetra4 :: Object
    wall_bt = initObject (Plain ey3 0) m_wall -- bottom
    wall_tp = initObject (Plain (negate ey3) 4) m_wall
    wall_rt = initObject (Plain (negate ex3) 2) m_wallb
    wall_lt = initObject (Plain ex3 2) m_wallr
    wall_bk = initObject (Plain ez3 6) m_wall
    wall_ft = initObject (Plain (negate ez3) 5) m_wall
    ball1   = initObject (Sphere (initPos 0 0.8 3.0) 0.8) m_ball
    --ball1   = initObject (Sphere (initPos 1 0.8 2.0) 0.8) m_ball
    ball2   = initObject (Sphere (initPos (-1) 0.8 1.5) 0.8) m_ball2
    ball3   = initObject (Sphere (initPos 1 0.8 2.5) 0.8) m_ball

    box_u   = initObject (initParallelogram bp1 bp2 bp4) m_ball
    box_f   = initObject (initParallelogram bp1 bp4 bp5) m_ball
    box_i   = initObject (initParallelogram bp4 bp3 bp8) m_ball
    box_r   = initObject (initParallelogram bp2 bp6 bp3) m_ball
    box_b   = initObject (initParallelogram bp7 bp6 bp8) m_ball
    box_l   = initObject (initParallelogram bp1 bp5 bp2) m_ball
    tetra1  = initObject (initPolygon (tetra!!0) (tetra!!3) (tetra!!1)) m_ball
    tetra2  = initObject (initPolygon (tetra!!0) (tetra!!2) (tetra!!3)) m_ball
    tetra3  = initObject (initPolygon (tetra!!0) (tetra!!1) (tetra!!2)) m_ball
    tetra4  = initObject (initPolygon (tetra!!3) (tetra!!2) (tetra!!1)) m_ball

    paralgt = initObject (Parallelogram (initPos (-0.5) 3.99 2.5) (negate ey3) ex3 ez3) m_paral
    --paralgt = initObject (Parallelogram (initPos (-1.99) 3.99 3.99) (negate ey3) ez3 ex3) m_sunl
-}
