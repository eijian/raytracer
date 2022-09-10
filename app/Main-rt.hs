--
-- Ray tracer w/Photon map
--
-- compile: ghc -o rt RT.hs
-- usage  : ./rt [screen info file] < [photonmapfile] > [imagefile.ppm]

module Main where

--import Data.List
--import           Control.Monad
--import qualified Data.Time    as TM
--import qualified Data.Text    as T
--import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import           System.Environment
--import           System.IO

import PhotonMap
import Scene
import Screen
import Tracer

usage :: String
usage = "Usage: rt [screen info file] < [photon map file]"

-- FUNCTIONS --

main :: IO ()
main = do
  -- read scene information
  args <- getArgs
  (fn1, fn2) <- if length args == 2
    then return (args !! 0, args !! 1)
    else error usage
  scr <- readScreen fn1
  (mate_air, lgts, objs) <- readScene fn2

  -- read photon map
  --t0 <- TM.getCurrentTime
  maps0 <- readMap (mapDivision scr) (nSamplePhoton scr) (radius scr)
  --hPutStr   stderr ("finished reading map:" ++ (show msize) ++ " photons, ")
  --t1 <- TM.getCurrentTime
  --hPutStrLn stderr (show (TM.diffUTCTime t1 t0))

  -- tracing image
  let
    (_, maps) = V.unzip maps0
    filter = pfilter scr
    r = radius scr
    tracer = traceRay filter objs lgts 0 maps r mate_air mate_air
  rays <- V.mapM (generateRay scr) $ screenMap scr
  image <- V.mapM tracer rays

  -- output image data with/without anti-aliasing
  mapM_ putStrLn $ pnmHeader scr
  -- progressive mode is default
  --if (progressive scr) == True
  --    then

  V.mapM_ (putStrLn.radianceToString) image
  
  --  else do
  --    let pixels = V.map (radianceToRgb scr) image
  --    forM_ [0..(V.length pixels - 1)] $ \i -> do
  --      rgb <- smooth tracer scr pixels i
  --      --TIO.putStrLn $ rgbToText rgb
  --      putStrLn $ rgbToString rgb
