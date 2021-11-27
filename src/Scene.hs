{-# LANGUAGE NoImplicitPrelude #-}

--
-- Scene
--

module Scene (
  m_air
, readScene
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
m_air = Material radiance0 white (Color 1.0 1.0 1.0)
  (initSurfaceSimple white black 0.0 0.0 0.0)
--
-- PUBLIC
--

readScene :: String -> IO (V.Vector Light, V.Vector Object)
readScene file = do
  lines <- readConfig file
  parseConfig ((intercalate "\n" lines) ++ "\n")

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

    mball = Material radiance0 (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0)
      (initSurfaceSimple (Color 0.5 0.5 0.5) (Color 0.0 0.0 0.0) 0.5 0.0 0.0)
    mwall = Material radiance0 (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0)
      (initSurfaceSimple (Color 0.5 0.5 0.5) (Color 0.8 0.8 0.8) 1.0 0.0 0.0)
    mwallr = Material radiance0 (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0)
      (initSurfaceSimple (Color 0.4 0.1 0.1) (Color 0.0 0.0 0.0) 1.0 0.0 0.0)
    mwallb = Material radiance0 (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0)
      (initSurfaceSimple (Color 0.1 0.1 0.4) (Color 0.0 0.0 0.0) 1.0 0.0 0.0)
    mparal = Material (Radiance 0.7958 0.7958 0.7958) (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0)
      (initSurfaceSimple (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0) 0.0 0.0 0.0)
    glass = Material radiance0 (Color 0.0 0.0 0.0) (Color 2.0 2.0 2.0)
      (initSurfaceSimple (Color 0.0 0.0 0.0) (Color 0.08 0.08 0.08) 0.0 0.0 0.0)
    silver = Material radiance0 (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0)
      (initSurfaceSimple (Color 0.0 0.0 0.0) (Color 0.78 0.78 0.78) 0.0 1.0 0.0)
    
    flooring = Object (Plain (Vector3 0.0 1.0 0.0) 0.0) mwall
    ceiling  = Object (Plain (Vector3 0.0 (-1.0) 0.0) 4.0) mwall
    rsidewall = Object (Plain (Vector3 (-1.0) 0.0 0.0) 2.0) mwallb
    lsidewall = Object (Plain (Vector3 1.0 0.0 0.0) 2.0) mwallr
    backwall = Object (Plain (Vector3 0.0 0.0 1.0) 6.0) mwall
    frontwall = Object (Plain (Vector3 0.0 0.0 (-1.0)) 5.0) mwall

    ball_glass = Object (Sphere (Vector3 1.0 0.7 2.6) 0.7) glass
    ball_silver = Object (Sphere (Vector3 (-0.9) 0.7 3.8) 0.7) silver
    ceiling_light = Object (initParallelogram (Vector3 (-0.67) 3.99 2.33)
      (Vector3 0.67 3.99 2.33) (Vector3 (-0.67) 3.99 3.67)) mparal

    os = [flooring, ceiling, rsidewall, lsidewall, backwall, frontwall,
      ball_glass, ball_silver, ceiling_light]

  return (V.fromList ls, V.fromList os)
