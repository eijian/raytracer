--
-- Ray tracer w/Photon map
--
-- compile: ghc -o rt RT.hs
-- usage  : ./rt < photonmapfile > imagefile.ppm

module Main where

--import Data.List
import Control.Monad
import System.IO
import Data.KdTree.Static
import qualified Data.Vector as V

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
  let
    tracer = traceRay 0 power photonmap objs
  outputHeader
  forM_ yline $ \y -> do
    let
      line = oneLine y
    image <- V.mapM tracer $ V.map generateRay' line
    outputImage image
    
--calcRadiance :: (Ray -> Radiance) -> [Radiance] -> Ray -> [Radiance]
--calcRadiance f rads ray = (f ray) : rads

