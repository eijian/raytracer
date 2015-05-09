{-# LANGUAGE NoImplicitPrelude #-}

--
-- Photon map generator
--

module Main where

import Ray.Algebra
import Ray.Physics
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

