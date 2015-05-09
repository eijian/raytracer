{-# LANGUAGE NoImplicitPrelude #-}

--
-- Physics
--

module Ray.Physics where

import System.Random
import Data.Maybe

import Ray.Algebra
import Ray.Geometry

--
-- Wavelength & Photon

data Wavelength = Red | Green | Blue deriving (Show, Read, Enum)

type Photon = (Wavelength, Ray)

initPhoton :: Wavelength -> Ray -> Photon
initPhoton l r = (l, r)

type PhotonCache = Photon

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

--
-- Light

pi2 = 2 * pi

type Flux = Double

data Light = PointLight Color Flux Position3

instance Show Light where
  show (PointLight c f p) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "]"

flux :: Light -> Flux
flux (PointLight _ f _) = f

generatePhoton :: Light -> IO Photon
generatePhoton (PointLight c _ p) = do
  theta <- randomRIO (0, pi)
  phi   <- randomRIO (0, pi2)
  wl    <- randomRIO (0, 1.0)
  let d = initDirFromAngle theta phi
      r = initRay p (fromJust d)
      w = decideWavelength c wl
  return (w, r)


