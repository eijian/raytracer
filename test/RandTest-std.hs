{-# LANGUAGE NoImplicitPrelude #-}
--
-- Random test (MT method)
--

module Main where

import Control.Monad
import NumericPrelude
import Ray.Algebra
import Ray.Optics
import Ray.Light
import Ray.Physics

plgt = PointLight (Color 1 1 1) 2.0 (Vector3 0 0 0)
nvec = 100000 :: Int

main :: IO ()
main = do
  putStrLn $ show nvec
  putStrLn $ show 2.0e-5
  forM_ [1..nvec] $ \i -> do
    v <- generatePhoton plgt
    let pi = convertToInfo v
    putStrLn ("(Red,(" ++ (show $ photonDir pi) ++ "," ++ (show $ Vector3 0 0 0) ++ "))")
    putStrLn $ show v
{--
  forM_ [1..5] $ \i -> do
    v <- generateRandomDir3
    putStrLn $ show v
--}
