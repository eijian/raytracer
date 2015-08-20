--
-- Ray tracer w/Photon map
--

module Main where

import Scene
import Screen
import Tracer

-- FUNCTIONS --

main :: IO ()
main = do
  (power, photonmap) <- readMap
  let image = map (traceRay 0 power photonmap objs) $ map generateRay' scrmap
  outputImage image

