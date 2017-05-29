{-# LANGUAGE NoImplicitPrelude #-}

--
-- Light
--

module Ray.Light (
  Light (PointLight, ParallelogramLight)
, flux
, generatePhoton
, getDirection
, getRadiance
) where

--import System.Random
import System.Random.Mersenne as MT
--import Data.Maybe
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Physics
import Ray.Optics

type Flux = Double

data Light = PointLight Color Flux Position3
           | ParallelogramLight Color Flux Position3
                                Direction3 Direction3 Direction3

instance Show Light where
  show (PointLight c f p) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "]"
  show (ParallelogramLight c f p n d1 d2) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "," ++ show n ++ "," ++ show d1 ++ "," ++ show d2 ++ "]"

flux :: Light -> Flux
flux (PointLight _ f _) = f
flux (ParallelogramLight _ f _ _ _ _) = f

generatePhoton :: Light -> IO Photon
generatePhoton (PointLight c _ p) = do
  --wl <- randomRIO (0, 1.0)
  wl <- MT.randomIO :: IO Double
  d  <- generateRandomDir4
  let r = initRay p d
      w = decideWavelength c wl
  return (w, r)
generatePhoton (ParallelogramLight c _ p n d1 d2) = do
  wl <- MT.randomIO :: IO Double
  t1 <- MT.randomIO :: IO Double
  t2 <- MT.randomIO :: IO Double
  d0 <- generateRandomDir4
  let d = if (d0 <.> n) < 0 then negate d0 else d0
      r = initRay (p + t1 *> d1 + t2 *> d2) d
      w = decideWavelength c wl
  return (w, r)

pi4 :: Double
pi4 = 4 * pi -- for decay by distance (1/ 4pi) 

getDirection :: Light -> Position3 -> Direction3
getDirection (PointLight _ _ lp) p = lp - p
getDirection (ParallelogramLight _ _ lp _ d1 d2) p = c - p  -- dummy
  where
    t = 0.5 :: Double
    c = p + t *> d1 + t *> d2

getRadiance :: Light -> Position3 -> Radiance
getRadiance l@(PointLight (Color r g b) f _) p
  | r2 == 0 = radiance0
  | otherwise = Radiance (r * l0) (g * l0) (b * l0)
  where
    r2 = square $ getDirection l p
    l0 = f / (pi4 * r2)
getRadiance l@(ParallelogramLight (Color r g b) f _ _ _ _) p  -- dummy
  | r2 == 0 = radiance0
  | otherwise = Radiance (r * l0) (g * l0) (b * l0)
  where
    r2 = square $ getDirection l p
    l0 = f / (pi4 * r2)

