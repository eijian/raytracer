{-# LANGUAGE NoImplicitPrelude #-}

--
-- Physics
--

module Ray.Physics where

import System.Random
import Data.Maybe
import NumericPrelude

import Ray.Algebra
import Ray.Geometry

--
-- Wavelength

data Wavelength = Red | Green | Blue deriving (Show, Read, Enum, Eq)

--
-- Color

data Color = Color Double Double Double

instance Show Color where
  show (Color r g b) = "[" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "]"

instance Eq Color where
  (Color ar ag ab) == (Color br bg bb) = (ar == br) && (ag == bg) && (ab == bb)

initColor :: Double -> Double -> Double -> Color
initColor r g b
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
decideWavelength (Color r g b) p
  | p < r     = Red
  | p < r + g = Green
  | otherwise = Blue


