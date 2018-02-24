--
-- Photon map generator (revision 2)
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm > photonmapfile

module Main where

import Control.Monad

import Ray.Light
import Ray.Object
import Tracer
import Screen
import Scene

main :: IO ()
main = do
  (lgts, objs) <- readScene ""
  scr <- readScreen ""
  putStrLn $ show $ nPhoton scr
  let power = (sum $ map flux lgts) / (fromIntegral $ nPhoton scr)
      ns    = map (calcN power) lgts
  putStrLn $ show power
  zipWithM_ (outputPhotonCaches (useClassicForDirect scr) objs) lgts ns
  
calcN :: Double -> Light -> Int
calcN power light = round (flux light / power)

outputPhotonCaches :: Bool -> [Object] -> Light -> Int -> IO ()
outputPhotonCaches uc objs lgt n = mapM_ (outputPhotonCache uc lgt objs) [1..n]

outputPhotonCache :: Bool -> Light -> [Object] -> Int -> IO ()
outputPhotonCache uc lgt objs _ =
  generatePhoton lgt >>= tracePhoton uc m_air objs 0 >>= mapM_ (putStrLn.show)

