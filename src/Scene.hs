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
    (ls, os0) = case (parse scene "rt scene file parse error" conf) of
      Left e -> error (show e)
      Right (l', o') -> (l', o')
    (n, os) = unzip os0
  return (ls, os)
