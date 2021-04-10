--
-- Ray tracer w/Photon map
--
-- compile: ghc -o rt RT.hs
-- usage  : ./rt [screen info file] < [photonmapfile] > [imagefile.ppm]

module Main where

--import Data.List
import           Control.Monad
import qualified Data.Time    as TM
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
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
  t0 <- TM.getCurrentTime
  (msize, photonmap) <- readMap (nSamplePhoton scr) (radius scr)
  hPutStr   stderr ("finished reading map:" ++ (show msize) ++ " photons, ")
  t1 <- TM.getCurrentTime
  hPutStrLn stderr (show (TM.diffUTCTime t1 t0))

  -- tracing image
  let tracer = traceRay scr m_air 0 photonmap objs lgts
  rays <- V.mapM (generateRay scr) $ screenMap scr
  image <- V.mapM tracer rays

  -- output image data with/without anti-aliasing
  mapM_ putStrLn $ pnmHeader scr
  if (progressive scr) == True
    then
      forM_ [0..(V.length image - 1)] $ \i -> do
        --TIO.putStrLn $ radianceToText (image V.! i)
        putStrLn $ radianceToString (image V.! i)
    else do
      let pixels = V.map (radianceToRgb scr) image
      forM_ [0..(V.length pixels - 1)] $ \i -> do
        rgb <- smooth tracer scr pixels i
        --TIO.putStrLn $ rgbToText rgb
        putStrLn $ rgbToString rgb
