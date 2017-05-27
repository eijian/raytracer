--
-- Ray tracer w/Photon map
--
-- compile: ghc -o rt RT.hs
-- usage  : ./rt < photonmapfile > imagefile.ppm

module Main where

import System.IO
import Data.KdTree.Static

import Scene
import Screen
import Tracer

-- FUNCTIONS --

main :: IO ()
main = do
  (power, photonmap) <- readMap
  hPutStrLn stderr ("finished reading map:" ++ (show $ size photonmap))
  let image = map (traceRay 0 power photonmap objs) $ map generateRay' scrmap
  hPutStrLn stderr ("finished drawing image:" ++ (show $ length image))
  outputImage image

