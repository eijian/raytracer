{-# LANGUAGE NoImplicitPrelude #-}

--
-- convert from photon map to image
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

import Screen

-- PARAMETERS --

xr, yr :: Int
xr = 2048
yr = 2048

----

main :: IO ()
main = do
  np <- getLine
  pw <- getLine
  let nphoton = read np :: Int
      power = read pw :: Double
  dat <- getContents
  pcs <- forM (lines dat) $ \i -> do
    return $ (read i :: PhotonCache)
  let cp  = target focus (initRay eyepos eyedir)
      sc  = Plain eyedir (negate eyedir <.> cp)
      map = getMap cp sc pcs
  a <- forM (map) $ \i -> do
    putStrLn $ show i
  return ()

getMap :: Position3 -> Shape -> [PhotonCache] -> [(Wavelength, Int, Int)]
getMap _ _ [] = []
getMap cp sc (pc:pcs)
  | t < focus = getMap cp sc pcs
  | px < 0 || px > (xr - 1) = getMap cp sc pcs
  | py < 0 || py > (yr - 1) = getMap cp sc pcs
  | otherwise = (getWl pc, px, py) : getMap cp sc pcs
  where
    d = (getPt pc) - eyepos
    d' = fromJust $ normalize d
    r = initRay eyepos d'
    t = head $ distance r sc
    p = (target t r) - cp
    --cos = ez3 <.> d'
    --t = focus / cos
    --p = eyepos + t *> d'
    px = round ((elemX p + 1.0) * (fromIntegral xr / 2))
    py = round ((1.0 - elemY p) * (fromIntegral yr / 2))

getPt :: PhotonCache -> Position3
getPt (_, (p, _)) = p

getWl :: PhotonCache -> Wavelength
getWl (wl, _) = wl
