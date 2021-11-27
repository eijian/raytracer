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
    power = (V.sum $ V.map flux lgts) / (fromIntegral $ nphoton scr)
    ns    = V.map (calcN power) lgts
  putStrLn $ show $ nphoton scr
  putStrLn $ show power
  V.zipWithM_ (outputPhotons (useClassicForDirect scr) objs) lgts ns
  
calcN :: Double -> Light -> Int
calcN power light = round (flux light / power)

outputPhotons :: Bool -> V.Vector Object -> Light -> Int -> IO ()
outputPhotons uc objs lgt n = V.mapM_ (outputPhoton uc lgt objs) $ V.replicate n 1

outputPhoton :: Bool -> Light -> V.Vector Object -> Int -> IO ()
outputPhoton uc lgt objs _ =
  generatePhoton lgt >>= tracePhoton uc objs 0 m_air >>= V.mapM_ (putStrLn.showPhoton)

showPhoton :: Photon -> String
showPhoton (wl, (Vector3 px py pz, Vector3 dx dy dz)) = show wl ++ " " 
  ++ show px ++ " " ++ show py ++ " " ++ show pz ++ " "
  ++ show dx ++ " " ++ show dy ++ " " ++ show dz
