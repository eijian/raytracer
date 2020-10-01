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

import Parser

--
-- CONSTANTS
--

m_air :: Material
m_air = Material radiance0 white white black (Color 1.0 1.0 1.0) 0.0 0.0 0.0

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
    (ls, os0) = case (parse scene "rt scene file parse error" conf) of
      Left e -> error (show e)
      Right (l', o') -> (l', o')
    (n, os) = unzip os0
  return (V.fromList ls, V.fromList os)
