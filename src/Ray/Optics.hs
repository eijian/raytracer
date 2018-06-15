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
, PhotonMap
, PhotonFilter (..)
, Radiance (Radiance)
, (<**>)
, rabs'
, elemR
, elemG
, elemB
, convertToInfo
, squaredDistance
, initPhoton
, nearest
, norm
, photonDir
, photonDummy
, photonInfoToRadiance
, photonPos
, power
, radiance0
, radiance1
, readMap
) where

import NumericPrelude
--import Debug.Trace
import qualified Data.KdTree.Static as KT
--import qualified Data.KdTree.Dynamic as KT

import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
import Test.QuickCheck

import Ray.Algebra
import Ray.Geometry
import Ray.Physics

--
-- Photon Filter
--

data PhotonFilter = Nonfilter
                  | Conefilter
                  | Gaussfilter
                  deriving (Eq, Ord, Show, Read)

--
-- Radiance

data Radiance = Radiance !Double !Double !Double
                deriving (Read, Show)

instance Eq Radiance where
  (Radiance r1 g1 b1) == (Radiance r2 g2 b2)
    = r1 == r2 && g1 == g2 && b1 == b2

instance Arbitrary Radiance where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ Radiance r g b

{- |

>>> let r1 = Radiance 0.01 0.02 0.005
>>> let r2 = Radiance 0.008 0.015 0.009
>>> norm (r1 - r2)
1.1e-2
-}
instance Additive.C Radiance where
  zero = Radiance 0 0 0
  (Radiance r1 g1 b1) + (Radiance r2 g2 b2)
    = Radiance (r1 + r2) (g1 + g2) (b1 + b2)
  (Radiance r1 g1 b1) - (Radiance r2 g2 b2)
    = Radiance (r1 - r2) (g1 - g2) (b1 - b2)

instance Module.C Double Radiance where
  s *> (Radiance r g b) = Radiance (s * r) (s * g) (s * b)

instance BasicMatrix Radiance where
  norm (Radiance r g b) = rabs r + rabs g + rabs b
  a .=. b = norm (a - b) < nearly0

rabs :: Double -> Double
rabs d = if d < 0.0 then (-d) else d

{- |
>>> rabs' (Radiance 0.001 (-0.02) (-0.00005))
Radiance 1.0e-3 2.0e-2 5.0e-5
-}

rabs' :: Radiance -> Radiance
rabs' (Radiance r g b) = Radiance (rabs r) (rabs g) (rabs b)

(<**>) :: Color -> Radiance -> Radiance
(Color cr cg cb) <**> (Radiance r g b)
  = Radiance (cr * r) (cg * g) (cb * b)
--infix 7 <**>
infix 8 <**>

elemRad :: Wavelength -> Radiance -> Double
elemRad Red   (Radiance r _ _) = r
elemRad Green (Radiance _ g _) = g
elemRad Blue  (Radiance _ _ b) = b

elemR :: Radiance -> Double
elemR = elemRad Red
elemG :: Radiance -> Double
elemG = elemRad Green
elemB :: Radiance -> Double
elemB = elemRad Blue

radiance0 :: Radiance
radiance0 = Radiance 0 0 0
radiance1 :: Radiance
radiance1 = Radiance 1 1 1

-- | Radiance
-- >>> let a = Radiance 0.3 0.8 0.3
-- >>> let b = Radiance 1.2 0.5 2.5
-- >>> a + b
-- Radiance 1.5 1.3 2.8

--
-- Photon 

type Photon = (Wavelength, Ray)

initPhoton :: Wavelength -> Ray -> Photon
initPhoton l r = (l, r)

type PhotonCache = Photon

data PhotonInfo = PhotonInfo !Wavelength !Position3 !Direction3
  deriving (Show, Eq)

data PhotonMap = PhotonMap
  { power :: Double
  , nearest :: PhotonInfo -> [PhotonInfo]
  }

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

squaredDistance :: PhotonInfo -> PhotonInfo -> Double
squaredDistance (PhotonInfo _ v1 _) (PhotonInfo _ v2 _) = d <.> d
  where
    d = v1 - v2

photonInfoToRadiance :: Direction3 -> Double -> PhotonInfo -> Radiance
photonInfoToRadiance n pw (PhotonInfo wl _ d)
  | wl == Red   = Radiance pw' 0 0
  | wl == Green = Radiance 0 pw' 0
  | wl == Blue  = Radiance 0 0 pw'
  where
    cos0 = n <.> d
    pw'  = if cos0 > 0.0 then pw * cos0 else 0.0
photonInfoToRadiance _ _ (PhotonInfo _ _ _) = radiance0

readMap :: Int -> IO (Int, PhotonMap)
readMap nsample = do
  --np' <- getLine
  _ <-getLine           -- discard infomation about the number of photon 
  pw' <- getLine
  ps <- getContents
  let
    --np = read np' :: Int
    pw = read pw' :: Double
    pcs = map convertToInfo (map (\x -> read x :: PhotonCache) (lines ps))
    pmap = KT.buildWithDist infoToPointList squaredDistance pcs
  return (KT.size pmap, PhotonMap pw (KT.kNearest pmap nsample))

infoToPointList :: PhotonInfo -> [Double]
infoToPointList (PhotonInfo _ (Vector3 x y z) _) = [x, y, z]
