{-# LANGUAGE NoImplicitPrelude #-}

--
-- Ray tracer classical
--

module Main where

import System.IO
import Control.Monad
import Data.KdTree.Static
import NumericPrelude
import Debug.Trace

import Ray.Algebra
import Ray.Geometry
import Ray.Object
import Ray.Optics
import Scene
import Screen
import Tracer

main :: IO ()
main = do
  let image = map (traceRay' 0 lgts objs) $ map generateRay' scrmap
  outputImage image



