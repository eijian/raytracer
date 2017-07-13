{-# LANGUAGE NoImplicitPrelude #-}

--
-- Antialias
--

module Antialias (
  smooth
) where

import qualified Data.Vector as V
import NumericPrelude

import Ray.Geometry
import Ray.Optics

import Screen

imgOffset :: [Int]
imgOffset = [-xres-1, -xres, -xres+1, -1, 1, xres-1, xres, xres+1]

smooth :: Bool -> (Ray -> IO Radiance) -> V.Vector Radiance -> Int
       -> Radiance
smooth False _ ims i = ims V.! i
smooth True tracer ims i
  | isDifferent i ims imgOffset == True = Radiance 0.0 1.0 0.0
  | otherwise = ims V.! i

{-

  IN : position in the vector
       vector of Radiance
       image offset
  OUT: true = have to trace a detail of the cell
       false = no need to detail tracing
-}

isDifferent :: Int -> V.Vector Radiance -> [Int] ->  Bool
isDifferent _ _ [] = False
isDifferent p rs (i:is)
  | p' <  0            = isDifferent p rs is
  | p' >= length rs    = isDifferent p rs is
  | df >= diffAliasing = True
  | otherwise          = isDifferent p rs is
  where
    p' = p + i
    df = norm (rs V.! p - rs V.! p')

