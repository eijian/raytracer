{-# LANGUAGE NoImplicitPrelude #-}

--
-- Light
--

module Ray.Light where

import System.Random
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
  wl <- randomRIO (0, 1.0)
  d  <- generateRandomDir2
  let r = initRay p d
      w = decideWavelength c wl
  return (w, r)
