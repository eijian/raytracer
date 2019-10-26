--
-- Ray tracer classical
--
-- compile: ghc -o rtc RTC.hs
-- usage  : ./rtc > imagefile.ppm

module Main where

import Control.Monad
import qualified Data.Vector as V

import Scene
import Screen
import Tracer
import Antialias

main :: IO ()
main = do
  (lgts, objs) <- readScene ""
  scr <- readScreen ""
  mapM_ putStrLn $ pnmHeader scr
  let tracer = traceRay' scr 0 lgts objs
  rays <- V.mapM (generateRay scr) $ screenMap scr
  image <- V.mapM tracer rays
  let cells = V.map (radianceToRgb scr) image
  forM_ [0..(V.length cells - 1)] $ \i -> do
    rgb <- smooth tracer scr cells i
    putStrLn $ rgbToString rgb

