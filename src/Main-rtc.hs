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

main :: IO ()
main = do
  outputHeader
{-
  forM_ yline $ \y -> do
    let
      line = oneLine y
      image = V.map (traceRay' 0 lgts objs) $ V.map generateRay' line
    outputImage image
-}
  let image = V.map (traceRay' 0 lgts objs) $ V.map generateRay' scrmap
  outputImage image
