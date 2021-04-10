--
-- Photon map generator (revision 2)
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm [screen info file] > photonmapfile

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.Vector as V
import System.Environment

import Ray.Algebra
import Ray.Light
import Ray.Object
import Ray.Optics
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
outputPhotonCaches uc objs lgt n = V.mapM_ (outputPhotonCache uc lgt objs) $ V.replicate n 1

outputPhotonCache :: Bool -> Light -> V.Vector Object -> Int -> IO ()
outputPhotonCache uc lgt objs _ =
  generatePhoton lgt >>= tracePhoton uc m_air objs 0 >>= V.mapM_ (putStrLn.showPhoton)

showPhoton :: PhotonCache -> String
showPhoton (wl, (Vector3 px py pz, Vector3 dx dy dz)) = show wl ++ " " 
  ++ show px ++ " " ++ show py ++ " " ++ show pz ++ " "
  ++ show dx ++ " " ++ show dy ++ " " ++ show dz
