--
-- Photon map generator (revision 2)
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm [screen info file] > photonmapfile

module Main where

import Control.Monad
import System.Environment

import Ray.Light
import Ray.Object
import Tracer
import Screen
import Scene

usage :: String
usage = "Usage: pm [screen info file] > [photon map file]"

main :: IO ()
main = do
  as <- getArgs
  fn <- if length as == 1
    then return $ head as
    else error usage
  scr <- readScreen fn
  (lgts, objs) <- readScene ""
  let
    nPhoton = 100000
    power = (sum $ map flux lgts) / (fromIntegral nPhoton)
    ns    = map (calcN power) lgts
  putStrLn $ show nPhoton
  putStrLn $ show power
  zipWithM_ (outputPhotonCaches (useClassicForDirect scr) objs) lgts ns
  
calcN :: Double -> Light -> Int
calcN power light = round (flux light / power)

outputPhotonCaches :: Bool -> [Object] -> Light -> Int -> IO ()
outputPhotonCaches uc objs lgt n = mapM_ (outputPhotonCache uc lgt objs) [1..n]

outputPhotonCache :: Bool -> Light -> [Object] -> Int -> IO ()
outputPhotonCache uc lgt objs _ =
  generatePhoton lgt >>= tracePhoton uc m_air objs 0 >>= mapM_ (putStrLn.show)

