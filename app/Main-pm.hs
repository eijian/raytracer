--
-- Photon map generator (revision 2)
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm [screen info file] > photonmapfile

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
--import           Debug.Trace
import           System.Environment

import Ray.Algebra
import Ray.Light
--import Ray.Object
import Ray.Optics
import Scene
import Screen
import Tracer

usage :: String
usage = "Usage: pm <screen file> <scene file>  (output photon map to stdout)"

main :: IO ()
main = do
  args <- getArgs
  (fn1, fn2) <- if length args == 2
    then return (args !! 0, args !! 1)
    else error usage
  scr <- readScreen fn1
  (mate_air, lgts, objs) <- readScene fn2
  let
    power = (V.sum $ V.map flux lgts) / (fromIntegral $ nphoton scr)
    ns = V.map (calcN power) lgts
    tracer = tracePhoton (useClassicForDirect scr) objs 0 mate_air mate_air
  putStrLn $ show $ nphoton scr
  putStrLn $ show power
  V.zipWithM_ (outputPhotons tracer) lgts ns
  
calcN :: Double -> Light -> Int
calcN pow lgt = round (flux lgt / pow)

outputPhotons :: (Photon -> IO (V.Vector Photon)) -> Light -> Int -> IO ()
outputPhotons tracer lgt n = V.mapM_ (outputPhoton tracer lgt) $ V.replicate n 1

outputPhoton :: (Photon -> IO (V.Vector Photon)) -> Light -> Int -> IO ()
outputPhoton tracer lgt _ =
  generatePhoton lgt >>= tracer >>= V.mapM_ (putStrLn.showPhoton)

showPhoton :: Photon -> String
showPhoton (wl, (Vector3 px py pz, Vector3 dx dy dz)) = show wl ++ " " 
  ++ show px ++ " " ++ show py ++ " " ++ show pz ++ " "
  ++ show dx ++ " " ++ show dy ++ " " ++ show dz
