{-# LANGUAGE NoImplicitPrelude #-}

--
-- Photon map test 2
--

module Main where

import Control.Monad
import Data.Maybe
import Ray.Algebra
import Ray.Geometry
import Ray.Physics

main :: IO ()
main = do
  np <- getLine
  pw <- getLine
  let nphoton = read np :: Int
      power = read pw :: Double
  --putStrLn $ show nphoton
  --putStrLn $ show power
  dat <- getContents
  pcs <- forM (lines dat) $ \i -> do
    return $ (read i :: PhotonCache)
  let map = getMap pcs
  a <- forM (map) $ \i -> do
    putStrLn $ show i
  return ()

eye = initPos 0 2 0
focus = 0.5
sc = Plain ez3 (-1.0)

getMap :: [PhotonCache] -> [(Wavelength, Int, Int)]
getMap [] = []
getMap (pc:pcs)
  | t < focus = getMap pcs
  | px < 0 || px > 255 = getMap pcs
  | py < 0 || py > 255 = getMap pcs
  | otherwise = (getWl pc, px, py) : getMap pcs
  where
    d = (getPt pc) - eye
    d' = fromJust $ normalize d
    r = initRay eye d'
    (t, s)  = head $ distance r sc
    p = target t r
    --cos = ez3 <.> d'
    --t = focus / cos
    --p = eye + t *> d'
    px = round ((elemX p + 1.0) * 128)
    py = round ((3.0 - elemY p) * 128)

getPt :: PhotonCache -> Position3
getPt (_, (p, _)) = p

getWl :: PhotonCache -> Wavelength
getWl (wl, _) = wl
