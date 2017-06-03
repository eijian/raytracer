--
-- Photon map generator (revision 2)
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm > photonmapfile

module Main where

import Control.Monad

import Ray.Light
import Tracer
import Scene

nphoton :: Int
nphoton = 200000

main :: IO ()
main = do
  putStrLn $ show nphoton
  let power = (sum $ map flux lgts) / (fromIntegral nphoton)
      ns    = map (calcN power) lgts
  putStrLn $ show power
  zipWithM_ outputPhotonCaches ns lgts
  
calcN :: Double -> Light -> Int
calcN power light = round (flux light / power)

outputPhotonCaches :: Int -> Light -> IO ()
outputPhotonCaches n lgt = mapM_ (outputPhotonCache lgt) [1..n]

outputPhotonCache :: Light -> Int -> IO ()
outputPhotonCache lgt _ =
  generatePhoton lgt >>= tracePhoton objs 0 >>= mapM_ (putStrLn.show)

