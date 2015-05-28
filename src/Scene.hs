{-# LANGUAGE NoImplicitPrelude #-}

--
-- Scene
--

module Scene where

import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Physics
import Ray.Object
import Ray.Material

pl1 = PointLight (initColor 1 1 1) 1.0 (initPos (-1.5) 3.5 4.5) -- 2W
pl2 = PointLight (initColor 2 1 0) 1.0 (initPos 1.5 3.5 4.5) -- 1W
lgts = [pl1, pl2]
--pl1 = PointLight (initColor 1 1 1) 2.0 (initPos 0 3.99 2.5) -- 2W
--lgts = [pl1]

m_ball = Material 0.8 0.3 0.3
m_wall = Material 0.8 0.8 0.8
m_ceil = Material 0.4 0.2 0.02
m_flor = Material 0.8 0.6 0.4

wall_bt = initObject (Plain ey3 0) m_flor -- bottom
wall_tp = initObject (Plain (negate ey3) 4) m_ceil
wall_rt = initObject (Plain (negate ex3) 2) m_wall
wall_lt = initObject (Plain ex3 2) m_wall
wall_bk = initObject (Plain ez3 1) m_wall
wall_ft = initObject (Plain (negate ez3) 5) m_wall
ball1   = initObject (Sphere (initPos 1 0.8 3) 0.8) m_ball

objs = [wall_bt, wall_tp, wall_rt, wall_lt, wall_bk, wall_ft, ball1]

