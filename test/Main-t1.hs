
module Main where

import VectorTest

nreps = 1000000 :: Int

main :: IO ()
main = do
  let v1s = replicate nreps (Vector3 2.1 4.5 8.2)
  let v2s = replicate nreps (Vector3 1.1 2.5 7.2)
  let ss  = replicate nreps 4.7

  putStrLn "test: elemZ"
  let a1s = map testElemZ v1s
  putItems a1s

  putStrLn "test: add"
  let a1s = map testAdd (zip v1s v2s)
  putItems a1s

  putStrLn "test: sub"
  let a2s = map testSub (zip v1s v2s)
  putItems a2s

  putStrLn "test: scale"
  let a3s = map testScale (zip ss v1s)
  putItems a3s

  putStrLn "test: dot"
  let a4s = map testDot (zip v1s v2s)
  putItems a4s

  putStrLn "test: cross"
  let a5s = map testCross (zip v1s v2s)
  putItems a5s

  putStrLn "test end"

putItems :: Show a => [a] -> IO ()
putItems vs = do
  putStrLn $ show vs

testElemZ :: Vector3 -> Double
testElemZ v = elemZ v

testAdd :: (Vector3, Vector3) -> Vector3
testAdd (v1, v2) = vadd v1 v2

testSub :: (Vector3, Vector3) -> Vector3
testSub (v1, v2) = vsub v1 v2

testScale :: (Double, Vector3) -> Vector3
testScale (s, v1) = vscale s v1

testDot :: (Vector3, Vector3) -> Double
testDot (v1, v2) = dot v1 v2

testCross :: (Vector3, Vector3) -> Vector3
testCross (v1, v2) = cross v1 v2


