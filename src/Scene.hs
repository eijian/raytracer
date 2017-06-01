{-# LANGUAGE NoImplicitPrelude #-}

--
-- Scene
--

module Scene (
  lgts
, objs
) where

import Data.Maybe
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Physics
import Ray.Optics
import Ray.Object
import Ray.Material

pl1 :: Light
--pl2 :: Light
lgts :: [Light]
--pl1 = PointLight (initColor 1 1 1) 5.0 (initPos 0 3.99 3.0) -- 2W
--pl1 = ParallelogramLight (initColor 1 1 1) 5.0 (initPos (-0.5) 3.99 2.5) (negate ey3) ex3 ez3  -- 5W Parallelogram
pl1 = SunLight (initColor 1 1 1) 20.0 (initPos (-1.99) 3.99 3.99) (negate ey3) ez3 ex3 (fromJust $ normalize $ Vector3 1 (-1) 1)  -- 10W Sun
--pl1 = PointLight (initColor 1 1 1) 1.0 (initPos (-1.5) 3.5 4.5) -- 1W
--pl2 = PointLight (initColor 2 1 0) 1.0 (initPos 1.5 3.5 4.5) -- 1W
lgts = [pl1]
--lgts = [pl1, pl2]

m_ball, m_wall, m_ceil, m_flor :: Material
m_ball = Material (Color 0.5 0.5 0.5)  black black radiance0 black 1.0 0.0
m_wall = Material (Color 0.5 0.5 0.5)  black black radiance0 black 1.0 0.0
m_ceil = Material (Color 0.4 0.2 0.02) black black radiance0 black 1.0 0.0
m_flor = Material (Color 0.5 0.3 0.1)  black black radiance0 black 1.0 0.0
m_wallr = Material (Color 0.4 0.0 0.0)  black black radiance0 black 1.0 0.0
m_wallb = Material (Color 0.0 0.0 0.4)  black black radiance0 black 1.0 0.0
pw = 5.0 / 1.0 / (2 * pi)
m_paral = Material black black black (Radiance pw pw pw) black 0.0 0.0

wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball1 :: Object
wall_bt = initObject (Plain ey3 0) m_wall -- bottom
wall_tp = initObject (Plain (negate ey3) 4) m_wall
wall_rt = initObject (Plain (negate ex3) 2) m_wallb
wall_lt = initObject (Plain ex3 2) m_wallr
wall_bk = initObject (Plain ez3 6) m_wall
wall_ft = initObject (Plain (negate ez3) 5) m_wall
ball1   = initObject (Sphere (initPos 0 0.8 3.0) 0.8) m_ball
--paralgt = initObject (Parallelogram (initPos (-0.5) 3.99 2.5) (negate ey3) ex3 ez3) m_paral
paralgt = initObject (Parallelogram (initPos (-1.99) 3.99 3.99) (negate ey3) ez3 ex3) m_paral

objs :: [Object]
objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball1, paralgt]

