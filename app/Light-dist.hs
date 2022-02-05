--
-- Light_dist: simulation of light distribution
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm [screen info file] > photonmapfile

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Monad
--import qualified Data.Vector as V
--import           Debug.Trace
import           System.Environment
import           NumericPrelude

import Ray.Algebra

usage :: String
usage = "Usage: pm <screen file> <scene file>  (output photon map to stdout)"

ndiv :: Int
ndiv = 200

theta :: Double
theta = pi / 2.0 / (fromIntegral ndiv)

nphoton :: Int
nphoton = 20000000

n :: Double
n = 1000000.0

main :: IO ()
main = do
  putStrLn (show ndiv)
  putStrLn (show nphoton)
  putStrLn (show n)
  let
    pow = 1.0 / (n + 1.0)
  forM_ [1..nphoton] $ \i -> do
    vec <- blurredVector ey3 pow
    let
      th = acos (ey3 <.> vec)
      n = floor (th / theta)
      n' = if n == ndiv then ndiv - 1 else n
    --putStrLn ("th=" ++ show th ++ ", theta=" ++ show theta)
    putStrLn (show n')



---
