{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

--
-- Photon
--

module Ray.Photon (
  Photon
, PhotonCache
, PhotonInfo (..)
, convertToInfo
, squaredDistance
, initPhoton
, photonDir
, photonDummy
, photonPos
) where

--import qualified Algebra.Additive as Additive
--import qualified Algebra.Module as Module
import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
--import Debug.Trace
--import           Data.List.Split
--import qualified Data.KdTree.Static as KT
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
--import qualified Data.Vector as V
import qualified Data.Vector.Generic.Base as VB
import           Data.Vector.Generic.Mutable hiding (basicLength)
import qualified Data.Vector.Unboxed as U
import           GHC.Generics 
import           NumericPrelude
--import           Test.QuickCheck

import           Ray.Algebra
import           Ray.Geometry
--import           Ray.KdTree as KT (buildWithDist, inRadius, kNearest, size)
import           Ray.Physics

type Photon = (Wavelength, Ray)

initPhoton :: Wavelength -> Ray -> Photon
initPhoton l r = (l, r)

type PhotonCache = Photon

data PhotonInfo = PhotonInfo !Wavelength !Position3 !Direction3
  deriving (Show, Read, Eq, Generic)
  --deriving (Show, Read, Eq, Generic, VB.Vector U.Vector, MVector U.MVector, U.Unbox)

--instance VB.Vector U.Vector PhotonInfo where
--  basicLength (U.Vector (PhotonInfo w p d)) = VB.basicLength w + VB.basicLength p + VB.basicLength d

instance NFData PhotonInfo where
  rnf = genericRnf

photonDummy :: Position3 -> PhotonInfo
photonDummy p = PhotonInfo Red p ex3

photonDir :: PhotonInfo -> Direction3
photonDir (PhotonInfo _ _ d) = d

photonPos :: PhotonInfo -> Position3
photonPos (PhotonInfo _ p _) = p

convertToInfo :: PhotonCache -> PhotonInfo
convertToInfo (wl, (rp, rd)) = PhotonInfo wl rp (negate rd)

squaredDistance :: PhotonInfo -> PhotonInfo -> Double
squaredDistance (PhotonInfo _ v1 _) (PhotonInfo _ v2 _) = d <.> d
  where
    d = v1 - v2

