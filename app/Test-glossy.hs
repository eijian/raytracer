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
--import           System.IO

import Ray.Algebra
--import Ray.Physics
--import Ray.Surface

n :: Int
n = 10000

m :: Double
m = 1.0 / (fromIntegral n)

main :: IO ()
main = do
  let
    nvec = ey3
    (_, pow) = densityPower 1.0
  putStrLn ("power=" ++ show pow)
  vs1 <- mapM (\_ -> blurredVector nvec pow) [1..n]
  let
    nvec' = fromJust $ normalize (m *> (foldl (+) o3 vs1))
    x = m * (foldl (+) 0 $ map elemX vs1)
    y = m * (foldl (+) 0 $ map elemY vs1)
    z = m * (foldl (+) 0 $ map elemZ vs1)
    cos  = nvec <.> nvec'
  putStrLn ("nvec=" ++ show nvec ++ ", nvec'=" ++ show nvec')
  putStrLn ("cos=" ++ show cos)
  putStrLn ("x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z)



--
