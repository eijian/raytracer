{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}


--
-- Physics
--

module Ray.Physics (
  Color (Color)
, Wavelength (Red, Green, Blue)
, black
, expColor
, initColor
, lowerThan
, normalizeColor
, decideWavelength
, selectWavelength
, relativeIorAverage
, relativeIorWavelength
, white
) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
import qualified Algebra.Ring as Ring
import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Maybe
import           GHC.Generics
import           NumericPrelude
import           Test.QuickCheck

import           Ray.Algebra

--
-- Wavelength

{-
波長
  Red  : 700.0 nm
  Green: 546.1 nm
  Blue : 435.5 nm
-}
data Wavelength = Red | Green | Blue deriving (Show, Read, Enum, Eq, Generic)

instance NFData Wavelength where
  rnf = genericRnf

--
-- Color

data Color = Color !Double !Double !Double
             deriving (Read, Generic)

instance NFData Color where
  rnf = genericRnf

instance Eq Color where
  (Color r1 g1 b1) == (Color r2 g2 b2)
    = r1 == r2 && g1 == g2 && b1 == b2

instance Show Color where
  show (Color r g b) = "[" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "]"

instance Arbitrary Color where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ Color r g b

instance Additive.C Color where
  zero = Color 0 0 0
  (Color r1 g1 b1) + (Color r2 g2 b2)
    = Color (r1 + r2) (g1 + g2) (b1 + b2)
  (Color r1 g1 b1) - (Color r2 g2 b2)
    = Color (r1 - r2) (g1 - g2) (b1 - b2)
  negate (Color r g b) = Color (1.0 - r) (1.0 - g) (1.0 - b)

instance Module.C Double Color where
  s *> (Color r g b) = Color (s * r) (s * g) (s * b)

instance Ring.C Color where
  (Color r1 g1 b1) * (Color r2 g2 b2)
    = Color (r1*r2) (g1*g2) (b1*b2)
  one = Color 1.0 1.0 1.0

black :: Color
black = Color 0.0 0.0 0.0
white :: Color
white = Color 1.0 1.0 1.0

initColor :: Double -> Double -> Double -> Color
initColor r g b = normalizeColor (Color r g b)

normalizeColor :: Color -> Color
normalizeColor (Color r g b)
  | mag == 0  = Color (1/3) (1/3) (1/3)
  | otherwise = Color (r'/mag) (g'/mag) (b'/mag)
  where
    r' = clipColor r
    g' = clipColor g
    b' = clipColor b
    mag = r' + g' + b'

clipColor :: Double -> Double
clipColor a
  | a < 0     = 0
  | otherwise = a

decideWavelength :: Color -> Double -> Wavelength
decideWavelength (Color r g _) p
  | p < r     = Red
  | p < r + g = Green
  | otherwise = Blue

selectWavelength :: Wavelength -> Color -> Double
selectWavelength Red   (Color r _ _) = r
selectWavelength Green (Color _ g _) = g
selectWavelength Blue  (Color _ _ b) = b

expColor :: Color -> Double -> Color
expColor c@(Color r g b) e
  | c == white = white
  | otherwise  = Color (r ** e) (g ** e) (b ** e)

lowerThan :: Color -> Color -> Bool
lowerThan (Color r1 g1 b1) (Color r2 g2 b2) =  r1 < r2 && g1 < g2 && b1 < b2


-- Physics Lows -----------------

-- relative of IoR
--   n1: IoR of src
--   n2: IoR of dst
--   eta = n2 / n1

relativeIor :: Double -> Double -> Double
relativeIor ior1 ior2
  | ior1 == 0.0 = 1.0
  | otherwise   = ior2 / ior1

{-
relativeIorColor :: Color -> Color -> Color
relativeIorColor (Color r1 g1 b1) (Color r2 g2 b2) =
  Color (relativeIor r1 r2) (relativeIor g1 g2) (relativeIor b1 b2)
-}

relativeIorWavelength :: Color -> Color -> Wavelength -> Double
relativeIorWavelength (Color r1 g1 b1) (Color r2 g2 b2) wl =
  case wl of
    Red   -> relativeIor r1 r2
    Green -> relativeIor g1 g2
    Blue  -> relativeIor b1 b2

relativeIorAverage :: Color -> Color -> Double
relativeIorAverage (Color r1 g1 b1) (Color r2 g2 b2) =
  relativeIor a1 a2
  where
    a1 = (r1 + g1 + b1) / 3.0
    a2 = (r2 + g2 + b2) / 3.0

{- |
Snell's low
  IN : r_ior = relative of IoR (n = n2/n1)
       nvec  = Normal vector (from surface)
       vvec  = eye direction (to surface)
  OUT: R  reflection dir
       T  refraction dir
       cos1  reflection cosine
       cos2  refraction cosine
-}

snell :: Double -> Direction3 -> Direction3
  -> (Direction3, Direction3, Double, Double)
snell r_ior nvec vvec
  | g <= 0.0  = (r, o3, c1, 0.0)   -- 全反射
  | otherwise = (r, t, c1, c2)
  where
    c1 = vvec <.> nvec
    r = fromJust $ normalize (vvec - (2.0 * c1) *> nvec)
    n = r_ior * r_ior
    g = 1.0 / n + c1 * c1 - 1.0

    a = -c1 - sqrt g
    t = fromJust $ normalize (r_ior *> (vvec + a *> nvec))
    c2 = sqrt (1.0 - n * (1.0 - c1 * c1))
