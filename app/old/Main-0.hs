{-# LANGUAGE NoImplicitPrelude #-}

--
-- Photon map generator
--

module Main where

import Control.Monad
import NumericPrelude

import Ray.Algebra
import Ray.Physics
import Ray.Light
import Tracer
import Scene


nphoton = 10000 :: Int

main :: IO ()
main = do
  let tflux = sum $ map flux lgts
  putStrLn $ show (tflux / (fromIntegral nphoton))
  let l = head lgts
  ps <- mapM generatePhoton (replicate nphoton l)
  prs <- mapM (tracePhoton objs) ps
  a <- forM (concat prs) $ \i -> do
    putStrLn $ show i
  putStrLn "end"

