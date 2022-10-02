--
-- Photon map generator and Ray Tracer (revision 2)
--
-- compile: cabal build
-- usage  : ./ppm [screen info file] > photonmapfile

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
--import           Debug.Trace
import           System.Environment

import Ray.Light
import Ray.Material
import Ray.Object
import Ray.Optics
import Ray.Physics
import PhotonMap
import Scene
import Screen
import Tracer

usage :: String
usage = "Usage: ppm <screen file> <scene file>  (output image data to stdout)"

main :: IO ()
main = do
  args <- getArgs
  (fn1, fn2) <- if length args == 2
    then return (args !! 0, args !! 1)
    else error usage
  scr <- readScreen fn1
  (mate_air, lgts, objs) <- readScene fn2

  photonmap <- photonMap scr objs lgts mate_air
  traceRays scr objs lgts photonmap mate_air

-- Create Photon Map

photonMap :: Screen -> V.Vector Object -> V.Vector LightObject -> Material 
  -> IO PhotonMap
photonMap scr objs lgts mate_air = do
  (power, photons) <- tracePhotons scr objs lgts mate_air
  let (_, photonmap) = buildMap power (nSamplePhoton scr) (radius scr) (V.toList photons)
  return photonmap

tracePhotons :: Screen -> V.Vector Object -> V.Vector LightObject -> Material
  -> IO (Double, V.Vector Photon)
tracePhotons scr objs lgts mate_air = do
  let
    (ns, power) = calcNumPhotons lgts (nphoton scr)
    tracer = tracePhoton objs 0 mate_air mate_air
  plists <- V.zipWithM (recordPhotons tracer) lgts ns
  let photons = V.foldl' (V.++) V.empty plists
  return (power, photons)

recordPhotons :: ((Photon, RadEstimation) -> IO (V.Vector Photon)) -> LightObject -> Int -> IO (V.Vector Photon)
recordPhotons tracer lgt n = do
  plists <- V.mapM (recordPhoton tracer lgt) $ V.replicate n 1
  return (V.foldl' (V.++) V.empty plists)

recordPhoton :: ((Photon, RadEstimation) -> IO (V.Vector Photon)) -> LightObject -> Int -> IO (V.Vector Photon)
recordPhoton tracer lgt _ = do
  (lgtspec, sfpt) <- validLightSpec lgt
  photon <- generatePhoton lgtspec sfpt
  tracer photon

-- rendering by Ray Tracing

traceRays :: Screen -> V.Vector Object -> V.Vector LightObject -> PhotonMap -> Material -> IO ()
traceRays scr objs lgts photonmap mate_air = do
  let
    filter = pfilter scr
    r = radius scr
    tracer = traceRay filter objs lgts 0 photonmap r mate_air mate_air white
  rays <- V.mapM (generateRay scr) $ screenMap scr
  image <- V.mapM tracer rays

  mapM_ putStrLn $ pnmHeader scr
  V.mapM_ (putStrLn.radianceToString) image


