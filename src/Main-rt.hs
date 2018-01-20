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

import Scene
import Screen
import Tracer
import Antialias

-- FUNCTIONS --

main :: IO ()
main = do
  (power, photonmap) <- readMap
  hPutStrLn stderr ("finished reading map:" ++ (show $ size photonmap))
  mapM_ putStrLn $ pnmHeader
  let tracer = traceRay m_air 0 power photonmap objs lgts
  image <- V.mapM tracer $ V.map generateRay' scrmap
  let pixels = V.map radianceToRgb image
  forM_ [0..(V.length pixels - 1)] $ \i -> do
    rgb <- smooth antiAliasing tracer pixels i
    putStrLn $ rgbToString rgb
