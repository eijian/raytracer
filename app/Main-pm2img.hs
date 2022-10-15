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
import System.Environment

import Ray.Algebra
import Ray.Geometry
import Ray.Optics
import Ray.Physics

import Camera

-- PARAMETERS --

xr, yr :: Int
xr = 2048
yr = 2048

----

usage :: String
usage = "Usage: pm2img [camera info file] < [photon map file]"

main :: IO ()
main = do
  args <- getArgs
  fn <- if length args == 1
    then return (args !! 0)
    else error usage
  cam <- readCamera fn
  np <- getLine
  pw <- getLine
  let
    _np = read np :: Int
    _pw = read pw :: Double
  dat <- getContents
  pcs <- forM (lines dat) $ \i -> do
    return $ (read i :: PhotonCache)
  let
    cp  = target (focusDistance cam) (initRay (eyePos cam) (eyeDir cam))
    sc  = Plain (eyeDir cam) (negate (eyeDir cam) <.> cp)
    map = getMap cam cp sc pcs
  forM_ (map) $ \i -> do
    putStrLn $ show i
  return ()

getMap :: Camera -> Position3 -> Shape -> [PhotonCache]
   -> [(Wavelength, Int, Int)]
getMap _ _ _ [] = []
getMap cam cp sc (pc:pcs)
  | t < (focusDistance cam)         = next
  | px < 0 || px > (xr - 1) = next
  | py < 0 || py > (yr - 1) = next
  | dist == Nothing         = next
  | otherwise = (getWl pc, px, py) : next
  where
    d = (getPt pc) - (eyePos cam)
    d' = fromJust $ normalize d
    r = initRay (eyePos cam) d'
    dist = distance r sc
    (_, t, _) = fromJust dist
    p = (target t r) - cp
    px = round ((elemX p + 1.0) * (fromIntegral xr / 2))
    py = round ((1.0 - elemY p) * (fromIntegral yr / 2))
    next = getMap cam cp sc pcs

getPt :: PhotonCache -> Position3
getPt (_, (p, _)) = p

getWl :: PhotonCache -> Wavelength
getWl (wl, _) = wl
