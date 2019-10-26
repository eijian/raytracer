--
-- Ray tracer w/Photon map
--
-- compile: ghc -o rt RT.hs
-- usage  : ./rt [screen info file] < [photonmapfile] > [imagefile.ppm]

module Main where

--import Data.List
import           Control.Monad
import qualified Data.Vector as V
import           System.Environment
import           System.IO

import Scene
import Screen
import Tracer
import Antialias

usage :: String
usage = "Usage: rt [screen info file] < [photon map file]"

-- FUNCTIONS --

main :: IO ()
main = do
  -- read scene information
  as <- getArgs
  (fn1, fn2) <- if length as == 2
    then return (as !! 0, as !! 1)
    else error usage
  scr <- readScreen fn1
  (lgts, objs) <- readScene fn2

  -- read photon map
  (msize, photonmap) <- readMap (nSamplePhoton scr) (radius scr)
  hPutStrLn stderr ("finished reading map:" ++ (show msize))

  -- tracing image
  let tracer = traceRay scr m_air 0 photonmap objs lgts
  rays <- V.mapM (generateRay scr) $ screenMap scr
  image <- V.mapM tracer rays

  -- output image data with/without anti-aliasing
  mapM_ putStrLn $ pnmHeader scr
  forM_ [0..(V.length image - 1)] $ \i -> do
    rad <- smooth2 tracer scr image i
    putStrLn $ radianceToString rad
  
