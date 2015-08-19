{-# LANGUAGE NoImplicitPrelude #-}

--
-- Ray tracer w/Photon map
--

module Main where

import System.IO
import Control.Monad
import Data.KdTree.Static
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Optics
import Scene
import Screen
import Tracer

-- FUNCTIONS --

main :: IO ()
main = do
  (power, photonmap) <- readMap
  let image = map (traceScreen power photonmap) scrmap
  outputImage image

readMap :: IO (Double, KdTree Double PhotonInfo)
readMap = do
  np' <- getLine
  pw' <- getLine
  let np = read np' :: Int
  let pw = read pw' :: Double
  pcs <- forM ([1..np]) $ \i -> do
    l <- getLine
    return $ (read l :: PhotonCache)
  let pmap = build infoToPointList (map convertToInfo pcs)
  return (pw, pmap)

traceScreen :: Double -> KdTree Double PhotonInfo -> (Int, Int) -> Radiance
traceScreen pw pmap cell = traceRay 0 pw pmap objs (generateRay' cell)
