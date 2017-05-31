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

paraDiv :: Double
paraDiv = 0.2
ts :: [Double]
ts = [0.1, 0.3, 0.5, 0.7, 0.9]
tss :: [(Double, Double)]
tss = [(x, y) | x <- ts, y <- ts]

getDirection :: Light -> Position3 -> [Direction3]
getDirection (PointLight _ _ lp) p = [lp - p]
getDirection (ParallelogramLight _ _ lp _ d1 d2) p =
  map (\(tx, ty) -> lp + tx *> d1 + ty *> d2 - p) tss

getRadiance :: Light -> [Double] -> [Radiance]
getRadiance _ [] = [radiance0]
getRadiance l@(PointLight (Color r g b) f _) (d:ds) =
  (Radiance (r * l0) (g * l0) (b * l0)) : getRadiance l ds
  where
    l0 = f / (pi4 * d)
getRadiance l@(ParallelogramLight (Color r g b) f _ _ _ _) (d:ds) =
  (Radiance (r * l0) (g * l0) (b * l0)) : getRadiance l ds
  where
    l0 = (f * paraDiv * paraDiv) / (pi4 * d)

