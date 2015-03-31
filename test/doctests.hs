--
--
--

module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/VectorTest.hs", "src/VectorTest2.hs", "src/VectorTest3.hs"]

