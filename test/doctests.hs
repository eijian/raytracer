--
--
--

module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Ray/Algebra.hs"]
-- main = doctest ["test/VectorTest.hs"]

