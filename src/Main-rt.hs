{-# LANGUAGE NoImplicitPrelude #-}

--
-- Ray tracer w/Photon map
--

module Main where


main :: IO ()
main = do
  np <- getLine
  pw <- getLine
  pcs <- forM ([1..np]) $ \i -> do
    getLine >>= (read :: PhotonCache)
