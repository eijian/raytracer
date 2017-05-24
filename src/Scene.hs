{-# LANGUAGE NoImplicitPrelude #-}

--
-- Scene
--

module Scene (
  lgts
, objs
) where

import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Physics
import Ray.Optics
import Ray.Object
import Ray.Material

--pl1 :: Light
--pl1 = PointLight (initColor 1 1 1) 2.0 (initPos 0 3.99 2.5) -- 2W
pl1, pl2 :: Light
pl1 = PointLight (initColor 1 1 1) 1.0 (initPos (-1.5) 3.5 4.5) -- 1W
pl2 = PointLight (initColor 2 1 0) 1.0 (initPos 1.5 3.5 4.5) -- 1W
lgts :: [Light]
--lgts = [pl1]
lgts = [pl1, pl2]

m_ball, m_wall, m_ceil, m_flor :: Material
m_ball = Material (Color 0.8 0.3 0.3)  black black radiance0 black 0.0
m_wall = Material (Color 0.8 0.8 0.8)  black black radiance0 black 0.0
m_ceil = Material (Color 0.4 0.2 0.02) black black radiance0 black 0.0
m_flor = Material (Color 0.8 0.6 0.4)  black black radiance0 black 0.0

wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball1 :: Object
wall_bt = initObject (Plain ey3 0) m_flor -- bottom
wall_tp = initObject (Plain (negate ey3) 4) m_ceil
wall_rt = initObject (Plain (negate ex3) 2) m_wall
wall_lt = initObject (Plain ex3 2) m_wall
wall_bk = initObject (Plain ez3 1) m_wall
wall_ft = initObject (Plain (negate ez3) 5) m_wall
ball1   = initObject (Sphere (initPos 1 0.8 3) 0.8) m_ball

objs :: [Object]
objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball1]

