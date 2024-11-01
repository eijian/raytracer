--
-- Ray tracer w/Photon map
--

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- compile: ghc -o rt RT.hs
-- usage  : ./rt [camera info file] < [photonmapfile] > [imagefile.ppm]

module Main where

--import qualified Data.Time    as TM
import qualified Data.Vector as V
import           System.Environment

import Ray.Physics
import Camera
import PhotonMap
import Scene
import Tracer

usage :: String
usage = "Usage: rt [camera info file] < [photon map file]"

-- FUNCTIONS --

main :: IO ()
main = do
  -- read scene information
  args <- getArgs
  (fn1, fn2) <- if length args == 2
    then return (args !! 0, args !! 1)
    else error usage
  cam <- readCamera fn1
  (mate_air, lgts, objs) <- readScene fn2 cam.whiteBalance

  -- read photon map
  --t0 <- TM.getCurrentTime
  (_msize, photonmap) <- readMap cam.radius
  --hPutStr   stderr ("finished reading map:" ++ (show msize) ++ " photons, ")
  --t1 <- TM.getCurrentTime
  --hPutStrLn stderr (show (TM.diffUTCTime t1 t0))

  -- tracing image
  let
    filter = cam.pfilter
    r = cam.radius
    tracer = traceRay filter objs lgts 0 photonmap r mate_air mate_air white
  rays <- V.mapM (generateRay cam) cam.screenMap
  image <- V.mapM tracer rays

  -- output image data with/without anti-aliasing
  mapM_ putStrLn cam.pnmHeader
  -- progressive mode is default
  --if (progressive cam) == True
  --    then
  let
    image' = V.map (compensateExposure cam) image
  V.mapM_ (putStrLn . radianceToString) image'
  
  --  else do
  --    let pixels = V.map (radianceToRgb cam) image
  --    forM_ [0..(V.length pixels - 1)] $ \i -> do
  --      rgb <- smooth tracer cam pixels i
  --      --TIO.putStrLn $ rgbToText rgb
  --      putStrLn $ rgbToString rgb
