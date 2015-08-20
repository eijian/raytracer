{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

--
-- Optics
--

module Ray.Optics (
  Photon
, PhotonCache
, PhotonInfo
, Radiance (Radiance)
, (<**>)
, convertToInfo
, infoToPointList
, photonDir
, photonDummy
, photonInfoToRadiance
, photonPos
, radiance0
) where

import Data.Maybe
import NumericPrelude
import Debug.Trace

import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
import Test.QuickCheck

import Ray.Algebra
import Ray.Geometry
import Ray.Physics

--
-- Radiance

data Radiance = Radiance Double Double Double deriving (Read, Show)

instance Eq Radiance where
  (Radiance ar ag ab) == (Radiance br bg bb)
    = ar == br && ag == bg && ab == bb

instance Arbitrary Radiance where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ Radiance r g b

instance Additive.C Radiance where
  zero = Radiance 0 0 0
  (Radiance ar ag ab) + (Radiance br bg bb)
    = Radiance (ar + br) (ag + bg) (ab + bb)
  (Radiance ar ag ab) - (Radiance br bg bb)
    = Radiance (ar - br) (ag - bg) (ab - bb)

instance Module.C Double Radiance where
  s *> (Radiance r g b) = Radiance (s * r) (s * g) (s * b)

instance BasicMatrix Radiance where
  norm (Radiance r g b) = r + g + b
  a .=. b = norm (a - b) < nearly0

(<**>) :: Color -> Radiance -> Radiance
(Color ar ag ab) <**> (Radiance br bg bb)
  = Radiance (ar * br) (ag * bg) (ab * bb)
infix 7 <**>

elemR :: Radiance -> Double
elemR (Radiance r _ _) = r
elemG :: Radiance -> Double
elemG (Radiance _ g _) = g
elemB :: Radiance -> Double
elemB (Radiance _ _ g) = g

radiance0 = Radiance 0 0 0

-- | Radiance
-- >>> let a = Radiance 0.1 0.8 0.3
-- >>> let b = Radiance 1.1 0.2 2.5
-- >>> a + b
-- Radiance 1 1 1

--
-- Photon 

type Photon = (Wavelength, Ray)

initPhoton :: Wavelength -> Ray -> Photon
initPhoton l r = (l, r)

type PhotonCache = Photon

data PhotonInfo = PhotonInfo Wavelength Position3 Direction3
  deriving (Show, Eq)

{-
instance Point PhotonInfo where
  dimension _ = 3
  coord 0 (PhotonInfo _ p _) = elemX p
  coord 1 (PhotonInfo _ p _) = elemY p
  coord 2 (PhotonInfo _ p _) = elemZ p
  dist2 (PhotonInfo _ p1 _) (PhotonInfo _ p2 _) = square (p1 - p2)
-}

photonDummy :: Position3 -> PhotonInfo
photonDummy p = PhotonInfo Red p ex3

photonDir :: PhotonInfo -> Direction3
photonDir (PhotonInfo _ _ d) = d

photonPos :: PhotonInfo -> Position3
photonPos (PhotonInfo _ p _) = p

convertToInfo :: PhotonCache -> PhotonInfo
convertToInfo (wl, (rp, rd)) = PhotonInfo wl rp (negate rd)

infoToPointList :: PhotonInfo -> [Double]
infoToPointList (PhotonInfo _ (Vector3 x y z) _) = [x, y, z]

photonInfoToRadiance :: Double -> PhotonInfo -> Radiance
photonInfoToRadiance pw (PhotonInfo Red   _ _) = Radiance pw 0 0
photonInfoToRadiance pw (PhotonInfo Green _ _) = Radiance 0 pw 0
photonInfoToRadiance pw (PhotonInfo Blue  _ _) = Radiance 0 0 pw

