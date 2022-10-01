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

readMap :: Int -> Int -> Double -> IO (V.Vector (Int, PhotonMap))
readMap mapdiv nsample radius = do
  _   <- T.getLine           -- discard infomation about the number of photon 
  pw0 <- T.getLine
  ps  <- T.getContents
  let
    pw = read (T.unpack pw0) :: Double
    lss0 = T.lines ps
  let
    nline = (length lss0 `div` mapdiv) + 1
    lss = V.fromList $ filter (\x -> length x > 0) $ splitEvery nline lss0
  -- フォトンマップを分割しようとしたが、ごく少数のフォトンの場合に
  -- うまく機能しないため１つのフォトンマップになるように戻した。
  --return $ V.map (buildMap pw nsample radius) lss
  return $ V.map (buildMap pw nsample radius) (V.fromList [lss0])

buildMap :: Double -> Int -> Double -> [T.Text] -> (Int, PhotonMap)
buildMap pw nsample radius ls = (msize, pmap)
  where
    pcs = map convertToInfo $ map readPhoton ls
    kdt = pcs `deepseq` KT.buildWithDist photonToPointList squaredDistance pcs
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
