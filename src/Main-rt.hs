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
  as <- getArgs
  fn <- if length as == 1
    then return $ head as
    else error usage
  scr <- readScreen fn
  (lgts, objs) <- readScene ""
  (msize, photonmap) <- readMap (nSamplePhoton scr)
  hPutStrLn stderr ("finished reading map:" ++ (show msize))
  mapM_ putStrLn $ pnmHeader scr
  let tracer = traceRay scr m_air 0 photonmap objs lgts
  image <- V.mapM tracer $ V.map (generateRay scr) $ screenMap scr
  let pixels = V.map (radianceToRgb scr) image
  forM_ [0..(V.length pixels - 1)] $ \i -> do
    rgb <- smooth tracer scr pixels i
    putStrLn $ rgbToString rgb
