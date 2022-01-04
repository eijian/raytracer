--
-- Photon map generator (revision 2)
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm [screen info file] > photonmapfile

{-# LANGUAGE OverloadedStrings #-}

module Main where

--import           Control.Monad
import qualified Data.Vector as V
--import           Debug.Trace
import           System.Environment

import Ray.Algebra
import Ray.Light
--import Ray.Object
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
  (m_air, lgts, objs) <- readScene fn2
  let
    --nPhoton = 100000
    power = (V.sum $ V.map flux lgts) / (fromIntegral $ nphoton scr)
    ns = V.map (calcN power) lgts
    tracer = tracePhoton (useClassicForDirect scr) objs 0 m_air m_air
  putStrLn $ show $ nphoton scr
  putStrLn $ show power
  V.zipWithM_ (outputPhotons tracer) lgts ns
  
calcN :: Double -> Light -> Int
calcN power light = round (flux light / power)

outputPhotons :: (Photon -> IO (V.Vector Photon)) -> Light -> Int -> IO ()
outputPhotons tracer lgt n = V.mapM_ (outputPhoton tracer lgt) $ V.replicate n 1

outputPhoton :: (Photon -> IO (V.Vector Photon)) -> Light -> Int -> IO ()
outputPhoton tracer lgt _ =
  generatePhoton lgt >>= tracer >>= V.mapM_ (putStrLn.showPhoton)

showPhoton :: Photon -> String
showPhoton (wl, (Vector3 px py pz, Vector3 dx dy dz)) = show wl ++ " " 
  ++ show px ++ " " ++ show py ++ " " ++ show pz ++ " "
  ++ show dx ++ " " ++ show dy ++ " " ++ show dz
