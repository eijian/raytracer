
module Main where

import Control.Monad (forM_)
import System.Posix.Time
import System.IO
import VectorTest3

ntry  = 10 :: Int
nloop = 2000000 :: Int

main :: IO ()
main = do
  hPutStrLn stderr "List Type"
  let v1 = initVec 2.1 4.5 8.2
  let v2 = initVec 1.1 2.5 7.2
  let s  = 4.7
  tryN "noop " (\x -> v1) ntry
  tryN "new  " (\x -> genVec x) ntry
  tryN "add  " (\x -> vadd (genVec x) v2) ntry
  tryN "sub  " (\x -> vsub (genVec x) v2) ntry
  tryN "scale" (\x -> vscale s (genVec x)) ntry
  tryN "dot  " (\x -> initVec (dot (genVec x) v2) 0 0) ntry
  tryN "cross" (\x -> cross (genVec x) v2) ntry

tryN :: String -> (Int -> Vector3) -> Int -> IO ()
tryN a f n = do
  t0 <- epochTime
  forM_ [1..n] $ \i -> do
    loopN a f
  t1 <- epochTime
  hPutStrLn stderr (a ++ "avg Time: " ++ show (t1 - t0))

loopN :: String -> (Int -> Vector3) -> IO ()
loopN a f = do
  forM_ [1..nloop] $ \i -> do
    let vs = f i
    putStrLn $ show vs
  -- hPutStrLn stderr (a ++ " Time: " ++ show (t1 - t0))

genVec :: Int -> Vector3
genVec i = initVec (fromIntegral i) 8.4 2.8

