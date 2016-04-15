{-# LANGUAGE NoImplicitPrelude #-}

--
-- Light
--

module Ray.Light (
  Light (PointLight)
, flux
, generatePhoton
, getDirection
, getRadiance
) where

import System.Random
import System.Random.Mersenne as MT
--import Data.Maybe
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Physics
import Ray.Optics

type Flux = Double

data Light = PointLight Color Flux Position3

instance Show Light where
  show (PointLight c f p) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "]"

flux :: Light -> Flux
flux (PointLight _ f _) = f

generatePhoton :: Light -> IO Photon
generatePhoton (PointLight c _ p) = do
  --wl <- randomRIO (0, 1.0)
  wl <- MT.randomIO :: IO Double
  d  <- generateRandomDir2
  let r = initRay p d
      w = decideWavelength c wl
  return (w, r)

pi4 :: Double
pi4 = 4 * pi -- for decay by distance (1/ 4pi) 

getDirection :: Light -> Position3 -> Direction3
getDirection (PointLight _ _ lp) p = lp - p

getRadiance :: Light -> Position3 -> Radiance
getRadiance l@(PointLight (Color r g b) f lp) p
  | r2 == 0 = radiance0
  | otherwise = Radiance (r * l0) (g * l0) (b * l0)
  where
    r2 = square $ getDirection l p
    l0 = f / (pi4 * r2)

