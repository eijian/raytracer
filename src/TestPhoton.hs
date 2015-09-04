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
  putStrLn $ show nphoton
  putStrLn "2.0e-5"
  forM_ [1..nphoton] $ \i -> do
    (wl, (p, d)) <- generatePhoton lgt
    putStrLn $ show (wl, ((pt + d), p))
