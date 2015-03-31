
module Main where

import Control.Monad (forM_)
import System.Posix.Time
import System.IO
import VectorTest2

nreps = 5000000 :: Int

main :: IO ()
main = do
  let v1 = (2.1, 4.5, 8.2)
  let v2 = (1.1, 2.5, 7.2)
  let s  = 4.7
  t00 <- epochTime
  loopN "noop " (\x -> v1)
  loopN "new  " (\x -> genVec x)
  loopN "add  " (\x -> vadd (genVec x) v2)
  loopN "dot  " (\x -> ((dot (genVec x) v2), 0, 0))
  loopN "cross" (\x -> cross (genVec x) v2)

tryN :: String -> (Int -> Vector3) -> Int -> IO ()
tryN a f n = do
  t0 <- epochTime
  forM_ [1..n] $ \i -> do
    loopN a f
  t1 <- epochTime
  hPutStrLn stderr (a ++ "avg Time: " ++ show (t1 - t0))

loopN :: String -> (Int -> Vector3) -> IO ()
loopN a f = do
  t0 <- epochTime
  forM_ [1..nreps] $ \i -> do
    let vs = f i
    putStrLn $ show vs
  t1 <- epochTime
  hPutStrLn stderr (a ++ " Time: " ++ show (t1 - t0))

genVec :: Int -> Vector3
genVec i = ((fromIntegral i), 8.4, 2.8)

