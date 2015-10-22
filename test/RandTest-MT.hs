{-# LANGUAGE NoImplicitPrelude #-}
--
-- Random test (MT method)
--

module Main where

import Control.Monad
import NumericPrelude
import Ray.Algebra

nvec = 10000000 :: Int

main :: IO ()
main = do
  vecs <- replicateM nvec generateRandomDir3
  let v0 = foldl (+) (Vector3 0 0 0) vecs
  putStrLn $ show v0
  --mapM_ (putStrLn.show) vecs
{--
  forM_ [1..5] $ \i -> do
    v <- generateRandomDir3
    putStrLn $ show v
--}


