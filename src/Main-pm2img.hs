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
import Ray.Physics
import Ray.Optics

import Screen

-- PARAMETERS --

xr, yr :: Int
xr = 2048
yr = 2048

----

usage :: String
usage = "Usage: pm2img [screen info file] < [photon map file]"

main :: IO ()
main = do
  as <- getArgs
  fn <- if length as == 1
    then return (as !! 0)
    else error usage
  scr <- readScreen fn
  np <- getLine
  pw <- getLine
  let nphoton = read np :: Int
      power = read pw :: Double
  dat <- getContents
  pcs <- forM (lines dat) $ \i -> do
    return $ (read i :: PhotonCache)
  let cp  = target (focus scr) (initRay (eyePos scr) (eyeDir scr))
      sc  = Plain (eyeDir scr) (negate (eyeDir scr) <.> cp)
      map = getMap scr cp sc pcs
  a <- forM (map) $ \i -> do
    putStrLn $ show i
  return ()

getMap :: Screen -> Position3 -> Shape -> [PhotonCache]
       -> [(Wavelength, Int, Int)]
getMap _ _ _ [] = []
getMap scr cp sc (pc:pcs)
  | t < (focus scr)         = next
  | px < 0 || px > (xr - 1) = next
  | py < 0 || py > (yr - 1) = next
  | otherwise = (getWl pc, px, py) : next
  where
    d = (getPt pc) - (eyePos scr)
    d' = fromJust $ normalize d
    r = initRay (eyePos scr) d'
    t = head $ distance r sc
    p = (target t r) - cp
    px = round ((elemX p + 1.0) * (fromIntegral xr / 2))
    py = round ((1.0 - elemY p) * (fromIntegral yr / 2))
    next = getMap scr cp sc pcs

getPt :: PhotonCache -> Position3
getPt (_, (p, _)) = p

getWl :: PhotonCache -> Wavelength
getWl (wl, _) = wl
