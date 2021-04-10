{-# LANGUAGE NoImplicitPrelude #-}
--
--
--

module Main where

import Control.Monad
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Physics
import Ray.Light
import Ray.Optics

nphoton = 100000 :: Int

lgt = PointLight (Color 0.33 0.33 0.34) 1.0 (Vector3 0 0 0)
pt = Vector3 0 2 2
main :: IO ()
main = do
  forM_ [1..nphoton] $ \i -> do
    (wl, (p, d)) <- generatePhoton lgt
    let p = sortByAngle d
    putStrLn $ show p

pi1_8 = pi / 8.0
cos1_8 = cos (1 * pi1_8)
cos2_8 = cos (2 * pi1_8)
cos3_8 = cos (3 * pi1_8)
cos4_8 = cos (4 * pi1_8)
cos5_8 = cos (5 * pi1_8)
cos6_8 = cos (6 * pi1_8)
cos7_8 = cos (7 * pi1_8)
cos8_8 = cos (8 * pi1_8)

sortByAngle :: Direction3 -> Int
sortByAngle d
  | cos > cos1_8 = 1
  | cos > cos2_8 = 2
  | cos > cos3_8 = 3
  | cos > cos4_8 = 4
  | cos > cos5_8 = 5
  | cos > cos6_8 = 6
  | cos > cos7_8 = 7
  | cos > cos8_8 = 8
  where
    cos = ey3 <.> d

