--
-- Photon map generator (revision 2)
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm [screen info file] > photonmapfile

module Main where

import Control.Monad
import qualified Data.Vector  as V
import System.Environment

import Ray.Light
import Ray.Object
import Tracer
import Screen
import Scene

usage :: String
usage = "Usage: pm <screen file> <scene file>  (output photon map to stdout)"

main :: IO ()
main = do
  as <- getArgs
  (fn1, fn2) <- if length as == 2
    then return (as !! 0, as !! 1)
    else error usage
  scr <- readScreen fn1
  (lgts, objs) <- readScene fn2
  let
    --nPhoton = 100000
    --power = (sum $ map flux lgts) / (fromIntegral nPhoton)
    power = (V.sum $ V.map flux lgts) / (fromIntegral $ nphoton scr)
    ns    = V.map (calcN power) lgts
  putStrLn $ show $ nphoton scr
  putStrLn $ show power
  V.zipWithM_ (outputPhotonCaches (useClassicForDirect scr) objs) lgts ns
  
calcN :: Double -> Light -> Int
calcN power light = round (flux light / power)

outputPhotonCaches :: Bool -> V.Vector Object -> Light -> Int -> IO ()
outputPhotonCaches uc objs lgt n = mapM_ (outputPhotonCache uc lgt objs) [1..n]

outputPhotonCache :: Bool -> Light -> V.Vector Object -> Int -> IO ()
outputPhotonCache uc lgt objs _ =
  generatePhoton lgt >>= tracePhoton uc m_air objs 0 >>= mapM_ (putStrLn.show)

