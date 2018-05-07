{-# LANGUAGE NoImplicitPrelude #-}

--
-- Photon map generator
--

module Main where

import Control.Monad
import NumericPrelude

--import Ray.Algebra
--import Ray.Physics
import Ray.Object
import Ray.Optics
import Ray.Light
import Tracer
import Scene

nphoton = 100000 :: Int

main :: IO ()
main = do
  (power, photons) <- generatePhotons nphoton lgts
  --photoncaches <- tracePhotons objs photons
  putStrLn $ show nphoton
  putStrLn $ show power
  --a <- forM (photoncaches) $ \i -> do
  a <- forM (photons) $ \i -> do
    putStrLn $ show i
  return ()


generatePhotons :: Int -> [Light] -> IO (Double, [Photon])
generatePhotons nphoton lights = do
  let power = (sum $ map flux lights) / (fromIntegral nphoton)
      ns    = map (calcN power) lights
  photons <- mapM (mapM generatePhoton) (zipWith replicate ns lights)
  return $ (power, concat photons)

calcN :: Double -> Light -> Int
calcN power light = round (flux light / power)

tracePhotons :: [Object] -> [Photon] -> IO [PhotonCache]
tracePhotons objs photons = do
  pcs <- mapM (tracePhoton objs) photons
  return $ concat pcs
