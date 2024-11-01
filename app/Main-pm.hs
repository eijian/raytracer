--
-- Photon map generator (revision 2)
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}


module Main where

import qualified Data.Vector as V
--import           Debug.Trace
import           System.Environment

import Ray.Algebra
import Ray.Light
import Ray.Object
import Ray.Optics
import Scene
import Camera
import Tracer

usage :: String
usage = "Usage: pm <camera file> <scene file>  (output photon map to stdout)"

main :: IO ()
main = do
  args <- getArgs
  (fn1, fn2) <- if length args == 2
    then return (args !! 0, args !! 1)
    else error usage
  cam <- readCamera fn1
  (mate_air, lgts, objs) <- readScene fn2 cam.whiteBalance
  let
    (ns, power) = calcNumPhotons lgts cam.nphoton
    tracer = tracePhoton objs 0 mate_air mate_air
  putStrLn $ show cam.nphoton
  putStrLn $ show power
  V.zipWithM_ (outputPhotons tracer) lgts ns

outputPhotons :: ((Photon, RadEstimation) -> IO (V.Vector Photon)) -> LightObject -> Int -> IO ()
outputPhotons tracer lgt n = V.mapM_ (outputPhoton tracer lgt) $ V.replicate n 1

outputPhoton :: ((Photon, RadEstimation) -> IO (V.Vector Photon)) -> LightObject -> Int -> IO ()
outputPhoton tracer lgt _ = do
  (lgtspec, sfpt) <- validLightSpec lgt
  photon <- generatePhoton lgtspec sfpt
  pmap <- tracer photon
  V.mapM_ (putStrLn . showPhoton) pmap

showPhoton :: Photon -> String
showPhoton (wl, (Vector3 px py pz, Vector3 dx dy dz)) = show wl ++ " " 
  ++ show px ++ " " ++ show py ++ " " ++ show pz ++ " "
  ++ show dx ++ " " ++ show dy ++ " " ++ show dz
