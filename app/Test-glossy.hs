{-# LANGUAGE NoImplicitPrelude #-}

--
-- Ray tracer w/Photon map
--
-- compile: ghc -o rt RT.hs
-- usage  : ./rt [screen info file] < [photonmapfile] > [imagefile.ppm]

module Main where

--import Data.List
import           Control.Monad
import           Data.Maybe
import           NumericPrelude
import           System.IO

import Ray.Algebra
import Ray.Physics
import Ray.Surface

n :: Int
n = 10000

m :: Double
m = 1.0 / (fromIntegral n)

main :: IO ()
main = do
  let
    nv1 = ey3
    --nv1 = fromJust (normalize $ Vector3 1.0 0.0 1.0)
    pw = densityPower 1.0
  putStrLn ("power=" ++ show pw)
  vs1 <- mapM (\_ -> distributedNormal nv1 pw) [1..n]
  let
    nv1' = fromJust $ normalize (m *> (foldl (+) o3 vs1))
    x = m * (foldl (+) 0 $ map elemX vs1)
    y = m * (foldl (+) 0 $ map elemY vs1)
    z = m * (foldl (+) 0 $ map elemZ vs1)
    cos  = nv1 <.> nv1'
  putStrLn ("nv1=" ++ show nv1 ++ ", nv1'=" ++ show nv1')
  putStrLn ("cos=" ++ show cos)
  putStrLn ("x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z)



--
