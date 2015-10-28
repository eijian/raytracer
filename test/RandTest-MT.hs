{-# LANGUAGE NoImplicitPrelude #-}
--
-- Random test (MT method)
--

module Main where

import Control.Monad
import NumericPrelude
import Ray.Algebra

plgt = PointLight (Color 1 1 1) 2.0 (Position3 0 0 0)
nvec = 100 :: Int

main :: IO ()
main = do
  forM_ [1..nvec] $ \i -> do
    v <- generatePhoton plgt
    putStrLn $ show v
{--
  forM_ [1..5] $ \i -> do
    v <- generateRandomDir3
    putStrLn $ show v
--}
