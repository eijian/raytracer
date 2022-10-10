--
-- Ray tracer classical
--
-- compile: ghc -o rtc RTC.hs
-- usage  : ./rtc > imagefile.ppm

module Main where

import Control.Monad
import qualified Data.Vector as V

import Antialias
import Camera
import Scene
import Tracer

main :: IO ()
main = do
  (lgts, objs) <- readScene ""
  cam <- readCamera ""
  mapM_ putStrLn $ pnmHeader cam
  let tracer = traceRay' cam 0 lgts objs
  rays <- V.mapM (generateRay cam) $ screenMap cam
  image <- V.mapM tracer rays
  let cells = V.map (radianceToRgb cam) image
  forM_ [0..(V.length cells - 1)] $ \i -> do
    rgb <- smooth tracer cam cells i
    putStrLn $ rgbToString rgb

