

--
-- Antialias
--

module Antialias (
  smooth
) where

import qualified Data.Vector as V

import Ray.Geometry
import Ray.Optics

smooth :: Bool -> (Ray -> IO Radiance) -> V.Vector Radiance -> Int
       -> Radiance
smooth False _ ims i = ims V.! i
smooth True tracer ims i
  | otherwise             = ims V.! i
