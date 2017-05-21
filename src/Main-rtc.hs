--
-- Ray tracer classical
--
-- compile: ghc -o rtc RTC.hs
-- usage  : ./rtc > imagefile.ppm

module Main where

import Scene
import Screen
import Tracer

main :: IO ()
main = do
  let image = map (traceRay' 0 lgts objs) $ map generateRay' scrmap
  outputImage image