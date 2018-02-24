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
  mapM_ putStrLn $ pnmHeader
  let tracer = traceRay' scr 0 lgts objs
  image <- V.mapM tracer $ V.map generateRay scrmap
  let cells = V.map radianceToRgb image
  forM_ [0..(V.length cells - 1)] $ \i -> do
    rgb <- smooth antiAliasing tracer scr cells i
    putStrLn $ rgbToString rgb

