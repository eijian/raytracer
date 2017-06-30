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
--import Data.KdTree.Dynamic
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
  outputHeader
  let tracer = traceRay 0 power photonmap objs lgts
  image <- V.mapM tracer $ V.map generateRay' scrmap
  let
    ps = convertToPixels image
  outputImage ps
