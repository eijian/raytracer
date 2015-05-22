{-# LANGUAGE NoImplicitPrelude #-}

--
-- Ray tracer w/Photon map
--

module Main where

import qualified Data.Trees.KdTree

eyepos = initPos 0 2 0)
eyedir = ez3
upper = ey3
focus = 1.0

eex = ex3
eey = negate ey3
stepx = 2.0 / 256
stepy = 2.0 / 256
origin = focus *> eyedir
  + ((-1.0) *> eex + 0.5 * stepx)
  - (1.0    *> eey + 0.5 * stepy)
scrmap = [(y, x) | y <- [0..255], x <- [0..255]]  

data PhotonInfo = PhotonInfo Wavelength Position3 Direction3

instance Point PhotonInfo where
  dimension _ = 3
  coord 0 (PhotonInfo _ p _) = elemX p
  coord 1 (PhotonInfo _ p _) = elemY p
  coord 2 (PhotonInfo _ p _) = elemZ p
  dist2 (PhotonInfo _ p1 _) (PhotonInfo _ p2 _) = square (p1 - p2)

main :: IO ()
main = do
  (power, photonmap) <- readMap
  let image = map traceCell scrmap
  outputImage image

readMap :: IO (Double, KdTree PhotonInfo)
readMap = do
  np <- getLine
  pw <- getLine
  pis <- forM ([1..np]) $ \i -> do
    pc <- (getLine >>= (read :: PhotonCache))
    return $ convertToInfo pc

convertToInfo :: PhotonCache -> PhotonInfo
convertToInfo (wl, (rp, rd)) = PhotonInfo wl rp (negate rd)

traceCell :: (Int, Int) -> Radiance
traceCell (y, x) = 

generateRay :: (Double, Double) -> Maybe Ray
generateRay (y, x) =
  initRay eyepos (origin + ((stepx * x) *> eex) + ((stepy * y) *> eey))



