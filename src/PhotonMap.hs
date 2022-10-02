{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

--
-- Optics
--

module PhotonMap (
  PhotonMap
, buildMap
, inradius
, nearest
, power
, readMap
) where

import           Control.DeepSeq
import           Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           NumericPrelude

import           Ray.Algebra
import qualified Ray.KdTree as KT (buildWithDist, inRadius, kNearest, size)
import           Ray.Optics
import           Ray.Physics

data PhotonMap = PhotonMap
  { power    :: Double
  , nearest  :: Photon -> [Photon]
  , inradius :: Photon -> V.Vector Photon
  }

readMap :: Int -> Double -> IO (Int, PhotonMap)
readMap nsample radius = do
  _   <- T.getLine           -- discard infomation about the number of photon 
  pw0 <- T.getLine
  ps  <- T.getContents
  let
    pw = read (T.unpack pw0) :: Double
    pcs = map readPhoton $ T.lines ps
  return $ buildMap pw nsample radius pcs

buildMap :: Double -> Int -> Double -> [Photon] -> (Int, PhotonMap)
buildMap pw nsample radius pcs = (msize, pmap)
  where
    pcs' = map convertToInfo pcs
    kdt = pcs' `deepseq` KT.buildWithDist photonToPointList squaredDistance pcs'
    msize = KT.size kdt
    pmap = PhotonMap pw (KT.kNearest kdt nsample) (KT.inRadius kdt $ sqrt radius)

readPhoton :: T.Text -> Photon
readPhoton photon = (wl, (Vector3 px py pz, Vector3 dx dy dz))
  where
    [wl0, px0, py0, pz0, dx0, dy0, dz0] = splitOn " " $ T.unpack $ T.strip photon
    wl = case wl0 of
      "Red"   -> Red
      "Green" -> Green
      "Blue"  -> Blue
      _       -> Red
    px = read px0 :: Double
    py = read py0 :: Double
    pz = read pz0 :: Double
    dx = read dx0 :: Double
    dy = read dy0 :: Double
    dz = read dz0 :: Double

photonToPointList :: Photon -> [Double]
photonToPointList (_, ((Vector3 x y z), _)) = [x, y, z]
