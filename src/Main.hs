{-# LANGUAGE NoImplicitPrelude #-}

--
-- main
--

module Main where

import Data.Maybe
import Ray.Algebra

main :: IO ()
main = do
  let a = initPos 1 2 3
  let b = initPos 1 (-1) 1
  putStrLn $ show (a + b)
  putStrLn $ show $ norm a
  putStrLn $ show (a <.> b)
  putStrLn $ show o3
  putStrLn $ show ex3
  putStrLn $ show ((1.2::Double) *> a)
  putStrLn $ show (b /> 3)
  let w = fromJust $ normalize b
  let r = w - (2 * (w <.> ey3)) *> ey3
  putStrLn $ show r
  


