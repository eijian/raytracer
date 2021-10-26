{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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

