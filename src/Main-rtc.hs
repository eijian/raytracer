--
-- Ray tracer classical
--
-- compile: ghc -o rtc RTC.hs
-- usage  : ./rtc > imagefile.ppm

module Main where

import qualified Data.Vector as V

import Scene
import Screen
import Tracer

main :: IO ()
main = do
  let image = V.map (traceRay' 0 lgts objs) $ V.map generateRay' scrmap
  outputImage image
