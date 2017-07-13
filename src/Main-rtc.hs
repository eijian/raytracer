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
  mapM_ putStrLn $ createHeader
  let
    tracer = traceRay' 0 lgts objs
  image <- V.mapM tracer $ V.map generateRay' scrmap
  forM_ [0..(V.length image - 1)] $ \i -> do
    putStrLn $ rgbToString.radianceToRgb $ smooth antiAliasing tracer image i
