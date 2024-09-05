{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}


--
-- Physics
--

module Ray.Physics (
  Color (Color)
, Wavelength (Red, Green, Blue)
, black
, decideWavelength
, expColor
, initColor
, initColorByKelvin
, initColorByKelvin2
, lowerThan
, minColor
, normalizeColor
, relativeIorAverage
, relativeIorWavelength
, selectWavelength
, snellLow
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
data Wavelength = Red | Green | Blue
  deriving (Show, Read, Enum, Eq, Generic)

instance NFData Wavelength where
  rnf :: Wavelength -> ()
  rnf = genericRnf

--
-- Color

data Color = Color !Double !Double !Double
  deriving (Read, Generic)

instance NFData Color where
  rnf :: Color -> ()
  rnf = genericRnf

instance Eq Color where
  (==) :: Color -> Color -> Bool
  (Color r1 g1 b1) == (Color r2 g2 b2)
    = r1 == r2 && g1 == g2 && b1 == b2

instance Show Color where
  show :: Color -> String
  show (Color r g b) = "[" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "]"

instance Arbitrary Color where
  arbitrary :: Gen Color
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ Color r g b

instance Additive.C Color where
  zero :: Color
  zero = Color 0 0 0
  (+) :: Color -> Color -> Color
  (Color r1 g1 b1) + (Color r2 g2 b2)
    = Color (r1 + r2) (g1 + g2) (b1 + b2)
  (-) :: Color -> Color -> Color
  (Color r1 g1 b1) - (Color r2 g2 b2)
    = Color (r1 - r2) (g1 - g2) (b1 - b2)
  negate :: Color -> Color
  negate (Color r g b) = Color (1.0 - r) (1.0 - g) (1.0 - b)

instance Module.C Double Color where
  (*>) :: Double -> Color -> Color
  s *> (Color r g b) = Color (s * r) (s * g) (s * b)

instance Ring.C Color where
  (*) :: Color -> Color -> Color
  (Color r1 g1 b1) * (Color r2 g2 b2)
    = Color (r1*r2) (g1*g2) (b1*b2)
  one :: Color
  one = Color 1.0 1.0 1.0

black :: Color
black = Color 0.0 0.0 0.0
white :: Color
white = Color 1.0 1.0 1.0
minColor :: Color
minColor = Color (1/10000) (1/10000) (1/10000)

initColor :: Double -> Double -> Double -> Color
initColor r g b = normalizeColor (Color r g b)

{- |
initColorByKelvin
  IN : color temperature (K)
  OUT: Color

  変換アルゴリズムは以下のサイトを参考にした。
  https://ja.visual-foxpro-programmer.com/color-temperature-rgb-conversion
  
-}

initColorByKelvin :: Double -> Color
initColorByKelvin t = normalizeColor (Color (clip r) (clip g) (clip b))
  where
    t' = t / 100.0
    r = if t' <= 66.0
      then 255.0
      else
        329.698727446 * ((t' - 60) ** (-0.1332047592))
    g = if t' <= 66.0
      then 99.4708025861 * logBase (exp 1) t' - 161.1195681661
      else 288.1221695283 * (t' - 60) ** (-0.0755148492)
    b = if t' >= 66.0
      then 255.0
      else 138.5177312231 * logBase (exp 1) (t' - 10) - 305.0447927307
    clip :: Double -> Double
    clip c = if c > 255.0
      then 1.0
      else c / 255.0

initColorByKelvin2 :: Double -> Color
initColorByKelvin2 t = normalizeColor $ kelvinTable !! t1
  where
    t'
      | t < 0.0     = 0.0
      | t > 12000.0 = 12000.0
      | otherwise   = t
    t1 = ceiling (t' / 100.0)

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

snellLow :: Double -> Direction3 -> Direction3
  -> (Direction3, Direction3, Double, Double)
snellLow r_ior nvec vvec
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

--
-- PRIVATE
--

-- URL: https://andi-siess.de/rgb-to-color-temperature/
kelvinTable :: [Color]
kelvinTable = [
    Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000
  , Color 1.000 0.220 0.000    -- 1000 K
  , Color 1.000 0.278 0.000
  , Color 1.000 0.325 0.000
  , Color 1.000 0.365 0.000
  , Color 1.000 0.396 0.000
  , Color 1.000 0.427 0.000
  , Color 1.000 0.451 0.000
  , Color 1.000 0.475 0.000
  , Color 1.000 0.494 0.000
  , Color 1.000 0.514 0.000
  , Color 1.000 0.541 0.071   -- 2000 K
  , Color 1.000 0.557 0.129
  , Color 1.000 0.576 0.173
  , Color 1.000 0.596 0.212
  , Color 1.000 0.616 0.247
  , Color 1.000 0.631 0.282
  , Color 1.000 0.647 0.310
  , Color 1.000 0.663 0.341
  , Color 1.000 0.678 0.369
  , Color 1.000 0.694 0.396
  , Color 1.000 0.706 0.420   -- 3000 K
  , Color 1.000 0.722 0.447
  , Color 1.000 0.733 0.471
  , Color 1.000 0.745 0.494
  , Color 1.000 0.757 0.518
  , Color 1.000 0.769 0.537
  , Color 1.000 0.780 0.561
  , Color 1.000 0.788 0.580
  , Color 1.000 0.800 0.600
  , Color 1.000 0.808 0.624
  , Color 1.000 0.820 0.639   -- 4000 K
  , Color 1.000 0.827 0.659
  , Color 1.000 0.835 0.678
  , Color 1.000 0.843 0.694
  , Color 1.000 0.851 0.714
  , Color 1.000 0.859 0.729
  , Color 1.000 0.867 0.745
  , Color 1.000 0.875 0.761
  , Color 1.000 0.882 0.776
  , Color 1.000 0.890 0.792
  , Color 1.000 0.894 0.808   -- 5000 K
  , Color 1.000 0.902 0.824
  , Color 1.000 0.910 0.835
  , Color 1.000 0.914 0.851
  , Color 1.000 0.922 0.863
  , Color 1.000 0.925 0.878
  , Color 1.000 0.933 0.890
  , Color 1.000 0.937 0.902
  , Color 1.000 0.941 0.914
  , Color 1.000 0.949 0.925
  , Color 1.000 0.953 0.937  --- 6000 K
  , Color 1.000 0.957 0.949
  , Color 1.000 0.961 0.961
  , Color 1.000 0.965 0.969
  , Color 1.000 0.973 0.984
  , Color 1.000 0.976 0.992
  , Color 0.996 0.976 1.000
  , Color 0.988 0.969 1.000
  , Color 0.976 0.965 1.000
  , Color 0.969 0.961 1.000
  , Color 0.961 0.953 1.000  --- 7000 K
  , Color 0.953 0.949 1.000
  , Color 0.941 0.945 1.000
  , Color 0.937 0.941 1.000
  , Color 0.929 0.937 1.000
  , Color 0.922 0.933 1.000
  , Color 0.914 0.929 1.000
  , Color 0.906 0.925 1.000
  , Color 0.902 0.922 1.000
  , Color 0.894 0.918 1.000
  , Color 0.890 0.914 1.000  -- 8000 K
  , Color 0.882 0.910 1.000
  , Color 0.878 0.906 1.000
  , Color 0.871 0.902 1.000
  , Color 0.867 0.902 1.000
  , Color 0.863 0.898 1.000
  , Color 0.855 0.898 1.000
  , Color 0.851 0.890 1.000
  , Color 0.847 0.890 1.000
  , Color 0.843 0.886 1.000
  , Color 0.839 0.882 1.000  -- 9000 K
  , Color 0.831 0.882 1.000
  , Color 0.827 0.878 1.000
  , Color 0.824 0.875 1.000
  , Color 0.820 0.875 1.000
  , Color 0.816 0.871 1.000
  , Color 0.812 0.867 1.000
  , Color 0.812 0.867 1.000
  , Color 0.808 0.863 1.000
  , Color 0.804 0.863 1.000
  , Color 0.812 0.855 1.000  -- 10000 K 
  , Color 0.812 0.855 1.000
  , Color 0.808 0.851 1.000
  , Color 0.804 0.851 1.000
  , Color 0.800 0.847 1.000
  , Color 0.800 0.847 1.000
  , Color 0.796 0.843 1.000
  , Color 0.792 0.843 1.000
  , Color 0.792 0.839 1.000
  , Color 0.788 0.839 1.000
  , Color 0.784 0.835 1.000  -- 11000 K
  , Color 0.784 0.835 1.000
  , Color 0.780 0.831 1.000
  , Color 0.776 0.831 1.000
  , Color 0.776 0.831 1.000
  , Color 0.773 0.827 1.000
  , Color 0.773 0.827 1.000
  , Color 0.773 0.824 1.000
  , Color 0.769 0.824 1.000
  , Color 0.765 0.824 1.000
  , Color 0.765 0.820 1.000  -- 12000 K
  ]