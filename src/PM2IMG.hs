{-# LANGUAGE NoImplicitPrelude #-}

--
-- Photon map test 2
--
-- compile: ghc -o pm2img PM2IMG.hs
-- usage  : ./pm2img < photonmapfile > imagesrcfile

module Main where

import Control.Monad
import Data.Maybe
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Physics
import Ray.Optics

-- PARAMETERS --

xres = 1024 :: Int
yres = 1024 :: Int

----

main :: IO ()
main = do
  np <- getLine
  pw <- getLine
  let nphoton = read np :: Int
      power = read pw :: Double
  --putStrLn $ show nphoton
  --putStrLn $ show power
  dat <- getContents
  --putStrLn "after getContents"
  pcs <- forM (lines dat) $ \i -> do
    return $ (read i :: PhotonCache)
  --putStrLn ("after read " ++ (show $ length pcs))
  let map = getMap pcs
  --putStrLn $ show map
  a <- forM (map) $ \i -> do
    --putStrLn "before show i"
    putStrLn $ show i
  return ()

eye = initPos 0 0 (-10)
--eye = initPos 0 2 0
focus = 0.5
sc = Plain ez3 (1.0)

getMap :: [PhotonCache] -> [(Wavelength, Int, Int)]
getMap [] = []
getMap (pc:pcs)
  | t < focus = getMap pcs
  | px < 0 || px > (xres - 1) = getMap pcs
  | py < 0 || py > (yres - 1) = getMap pcs
  | otherwise = (getWl pc, px, py) : getMap pcs
  where
    d = (getPt pc) - eye
    d' = fromJust $ normalize d
    r = initRay eye d'
    t = head $ distance r sc
    p = target t r
    --cos = ez3 <.> d'
    --t = focus / cos
    --p = eye + t *> d'
    px = round ((elemX p + 1.0) * (fromIntegral xres / 2))
    py = round ((1.0 - elemY p) * (fromIntegral yres / 2))

getPt :: PhotonCache -> Position3
getPt (_, (p, _)) = p

getWl :: PhotonCache -> Wavelength
getWl (wl, _) = wl
