--
--
--

module Main where

import Test.DocTest

main :: IO ()
--main = doctest ["src/Ray/Algebra.hs", "src/Ray/Optics.hs"]
main = doctest ["src/Ray/Optics.hs"]
-- main = doctest ["test/VectorTest.hs"]

