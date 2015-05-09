{-# LANGUAGE NoImplicitPrelude #-}

--
-- Photon map test
--

module Main where

import Control.Monad
import Ray.Algebra
import Ray.Physics

main :: IO ()
main = do
  np <- getLine
  pw <- getLine
  let nphoton = read np :: Int
      power = read pw :: Double
  putStrLn $ show nphoton
  putStrLn $ show power
  dat <- getContents
  pcs <- forM (lines dat) $ \i -> do
    return $ (read i :: PhotonCache)
  putStrLn $ show $ length pcs
  let tv = sumVectors pcs
  putStrLn $ show tv

sumVectors :: [PhotonCache] -> Position3
sumVectors [] = o3
sumVectors (pc:pcs) = getDirVec pc + sumVectors pcs

getDirVec :: PhotonCache -> Position3
getDirVec (_, (_, d)) = d

