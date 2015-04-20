{-# LANGUAGE NoImplicitPrelude #-}

--
-- main
--

module Main where

import NumericPrelude
import Data.Maybe
import Ray.Algebra

main :: IO ()
main = do
  let a = Vector3 1 2 3
  let b = Vector3 3 4 5
  let c = a + b
  putStrLn $ show c
  let d = norm a
  putStrLn $ show d
  let e = a <.> b
  putStrLn $ show e
  putStrLn $ show o3
  putStrLn $ show ex3
  putStrLn $ show ((1.2::Double) *> a)
  putStrLn $ show (b /> 3)
  let w = fromJust $ normalize b
  let r = w - (2 * (w <.> ex3)) *> ex3
  putStrLn $ show r
  


