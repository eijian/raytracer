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
  (lgts, objs) <- readScene ""
  scr <- readScreen ""
  (msize, photonmap) <- readMap (nSamplePhoton scr)
  hPutStrLn stderr ("finished reading map:" ++ (show msize))
  mapM_ putStrLn $ pnmHeader
  let tracer = traceRay scr m_air 0 photonmap objs lgts
  image <- V.mapM tracer $ V.map generateRay scrmap
  let pixels = V.map radianceToRgb image
  forM_ [0..(V.length pixels - 1)] $ \i -> do
    rgb <- smooth antiAliasing tracer scr pixels i
    putStrLn $ rgbToString rgb
