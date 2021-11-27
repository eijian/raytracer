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
, nearest
, inradius
, power
, readMap
) where

{-
import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
import           Debug.Trace
--import qualified Data.KdTree.Static as KT
import           Data.Vector.Generic.Base
import           Data.Vector.Generic.Mutable  hiding (read)
import qualified Data.Vector.Unboxed as VU
import           GHC.Generics 
import           Test.QuickCheck

--import           Ray.Geometry
-}

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
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
  , nearest  :: PhotonInfo -> [PhotonInfo]
  , inradius :: PhotonInfo -> V.Vector PhotonInfo
  }

readMap :: Int -> Double -> IO (Int, PhotonMap)
readMap nsample radius = do
--  hPutStrLn stderr "start reading"
{-
  _   <- getLine           -- discard infomation about the number of photon 
  pw0 <- getLine
  ps  <- getContents
  hPutStrLn stderr "after getContents"
  let
    pw = read pw0 :: Double
    ls = lines ps
  hPutStrLn stderr ("after lines" ++ show (length ls))
  let
    pcs = map convertToInfo (map (\x -> read x :: PhotonCache) ls)
-}
  _   <- T.getLine           -- discard infomation about the number of photon 
  pw0 <- T.getLine
  ps  <- T.getContents
--  hPutStrLn stderr "after getContents"
  let
    --np = read np' :: Int
    pw = read (T.unpack pw0) :: Double
    ls = V.fromList $ T.lines ps
  --hPutStrLn stderr ("after lines " ++ show (V.length ls))
  --hPutStrLn stderr ("LN:" ++ (show $ T.unpack (ls V.! 0)))
  let
    --pcs = V.toList $ V.map convertToInfo (V.map (\x -> read (T.unpack x) :: PhotonCache) ls)
    pcs = V.toList $ V.map convertToInfo (V.map (readPhoton) ls)
  
  --pcs `deepseq` hPutStrLn stderr "convert"
--  hPutStrLn stderr "convert"
  let
    --pmap = KT.buildWithDist infoToPointList squaredDistance pcs
    pmap = pcs `deepseq` KT.buildWithDist infoToPointList squaredDistance pcs
    --pmap = pcs `deepseq` fromList pcs
--  hPutStrLn stderr "after KT build"
  let
  --  msize = pmap `deepseq` KT.size pmap
    msize = KT.size pmap
    --msize = length pcs
  --hPutStrLn stderr ("radius= " ++ show radius)
  return (msize, PhotonMap pw (KT.kNearest pmap nsample) (KT.inRadius pmap $ sqrt radius))
  --return (msize, PhotonMap pw (kNearestNeighbors pmap nsample) (nearNeighbors pmap $ sqrt radius))

readPhoton :: T.Text -> Photon
readPhoton p = (wl, (Vector3 px py pz, Vector3 dx dy dz))
  where
    [wl0, px0, py0, pz0, dx0, dy0, dz0] = splitOn " " $ T.unpack $ T.strip p
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

infoToPointList :: PhotonInfo -> [Double]
infoToPointList (PhotonInfo _ (Vector3 x y z) _) = [x, y, z]
