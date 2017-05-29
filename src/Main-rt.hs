--
-- Ray tracer w/Photon map
--
-- compile: ghc -o rt RT.hs
-- usage  : ./rt < photonmapfile > imagefile.ppm

module Main where

--import Data.List
import System.IO
import Data.KdTree.Static

--import Ray.Geometry
--import Ray.Optics
import Scene
import Screen
import Tracer

-- FUNCTIONS --

main :: IO ()
main = do
  (power, photonmap) <- readMap
  hPutStrLn stderr ("finished reading map:" ++ (show $ size photonmap))
  let image = map (traceRay 0 power photonmap objs) $ map generateRay' scrmap
  outputImage image
{-
  let func = traceRay 0 power photonmap objs
      image = foldl' (calcRadiance func) [] $ map generateRay' scrmap
  outputImage $ reverse image
-}
  hPutStrLn stderr ("finished drawing image:" ++ (show $ length image))

--calcRadiance :: (Ray -> Radiance) -> [Radiance] -> Ray -> [Radiance]
--calcRadiance f rads ray = (f ray) : rads

