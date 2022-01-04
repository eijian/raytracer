{-# LANGUAGE NoImplicitPrelude #-}

--
-- Scene
--

module Scene (
  readScene
) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Physics
import Ray.Optics
import Ray.Object
import Ray.Material
import Ray.Surface

import Parser

--
-- CONSTANTS
--

m_air :: Material
--m_air = Material radiance0 white white black (Color 1.0 1.0 1.0) 0.0 0.0 0.0
m_air = initMaterial white 0.0 0.0 white (Color 1.0 1.0 1.0) Nothing

--
-- PUBLIC
--

readScene :: String -> IO (Material, V.Vector Light, V.Vector Object)
readScene file = do
  lines <- readConfig file
  (lgts, objs) <- parseConfig ((intercalate "\n" lines) ++ "\n")
  return (m_air, lgts, objs)

--
-- PRIVATE
--

readConfig :: String -> IO [String]
readConfig file = do
  f <- readFile file
  return $ map removeComment $ lines f

parseConfig :: String -> IO (V.Vector Light, V.Vector Object)
parseConfig conf = do
  let
    {-
    (ls, os0) = case (parse scene "rt scene file parse error" conf) of
      Left e -> error (show e)
      Right (l', o') -> (l', o')
    (n, os) = unzip os0
    -}
    ls = [ParallelogramLight (initColor 1.0 1.0 1.0) 5.0 (Vector3 (-0.67) 3.99 2.33)
      (Vector3 0.0 (-1.0) 0.0) (Vector3 1.33 0.0 0.0) (Vector3 0.0 0.0 1.33)]

    sf_wall = initSurface radiance0 1.0
    sf_paral = initSurface (Radiance 0.4421 0.4421 0.4421) 0.0
    sf_glass = initSurface radiance0 0.0
    sf_silver = initSurface radiance0 0.5
    --mball = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.0 1.0 1.0)
    --  (initSurfaceSimple (Color 0.5 0.5 0.5) (Color 0.0 0.0 0.0) 0.5 0.0 0.0)
    mwall = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
    mwallr = initMaterial (Color 0.4 0.1 0.1) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
    mwallb = initMaterial (Color 0.1 0.1 0.4) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
    -- 0.79578はflux 5.0 を半球（2πステラジアン）で割った値。<- 間違い
    --mparal = Material (Radiance 0.7958 0.7958 0.7958) (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0)
    -- emittanceは flux 5.0 / 半球 2π / ライト面積 1.34^2 = 0.4421。
    --   三原色それぞれがこの輝度を持つとした。
    mparal = initMaterial black 0.0 0.0 black black Nothing
    glass  = initMaterial (Color 1.0 1.0 1.0) 0.0 0.0 (Color 1.0 1.0 1.0) (Color 2.0 2.0 2.0) Nothing
    silver = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.96 0.76 0.39))
{-
    ypla00 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.0)
    ypla01 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.1)
    ypla02 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.2)
    ypla03 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.3)
    ypla04 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.4)
    ypla05 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.5)
    ypla06 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.6)
    ypla07 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.7)
    ypla08 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.8)
    ypla10 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 1.0)
-}
{-
    ypla00 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.7 1.7 1.7)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.08 0.08 0.08) 0.0 0.0 0.0)
    ypla01 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.1)
    ypla02 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.2)
    ypla03 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.3)
    ypla04 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.4)
    ypla05 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.5)
    ypla06 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.6)
    ypla07 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.7)
    ypla08 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.8)
    ypla10 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 1.0)
-}

    floor = Object (Plain (Vector3 0.0 1.0 0.0) 0.0) mwall sf_wall
    ceil  = Object (Plain (Vector3 0.0 (-1.0) 0.0) 4.0) mwall sf_wall
    rsidewall = Object (Plain (Vector3 (-1.0) 0.0 0.0) 2.0) mwallb sf_wall
    lsidewall = Object (Plain (Vector3 1.0 0.0 0.0) 2.0) mwallr sf_wall
    backwall = Object (Plain (Vector3 0.0 0.0 1.0) 6.0) mwall sf_wall
    frontwall = Object (Plain (Vector3 0.0 0.0 (-1.0)) 5.0) mwall sf_wall

    ball_glass = Object (Sphere (Vector3 1.0 0.7 2.6) 0.7) glass sf_glass
    ball_silver = Object (Sphere (Vector3 (-0.9) 0.7 3.8) 0.7) silver sf_silver
{-
    ball_01 = Object (Sphere (Vector3 (-1.6) 1.2 3.8) 0.4) ypla00
    ball_02 = Object (Sphere (Vector3 (-0.8) 1.2 3.8) 0.4) ypla01
    ball_03 = Object (Sphere (Vector3 ( 0.0) 1.2 3.8) 0.4) ypla02
    ball_04 = Object (Sphere (Vector3 ( 0.8) 1.2 3.8) 0.4) ypla03
    ball_05 = Object (Sphere (Vector3 ( 1.6) 1.2 3.8) 0.4) ypla04
    ball_06 = Object (Sphere (Vector3 (-1.6) 0.4 2.8) 0.4) ypla05
    ball_07 = Object (Sphere (Vector3 (-0.8) 0.4 2.8) 0.4) ypla06
    ball_08 = Object (Sphere (Vector3 ( 0.0) 0.4 2.8) 0.4) ypla07
    ball_09 = Object (Sphere (Vector3 ( 0.8) 0.4 2.8) 0.4) ypla08
    ball_10 = Object (Sphere (Vector3 ( 1.6) 0.4 2.8) 0.4) ypla10
-}
    --ball_silver = Object (Sphere (Vector3 (-0.9) 0.7 3.8) 0.7) mirror
    ceiling_light = Object (initParallelogram (Vector3 (-0.67) 3.99 2.33) 
      (Vector3 0.67 3.99 2.33) (Vector3 (-0.67) 3.99 3.67)) mparal sf_paral

    os = [floor, ceil, rsidewall, lsidewall, backwall, frontwall,
          ceiling_light,
          ball_glass, ball_silver]
--          ball_01, ball_02, ball_03, ball_04, ball_05,
--          ball_06, ball_07, ball_08, ball_09, ball_10]

      

  return (V.fromList ls, V.fromList os)
