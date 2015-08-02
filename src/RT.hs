{-# LANGUAGE NoImplicitPrelude #-}

--
-- Ray tracer w/Photon map
--

module Main where

import System.IO
import Control.Monad
import Data.KdTree.Static
import NumericPrelude

import Ray.Algebra
import Ray.Geometry

-- PARAMETERS --
-- for camera
eyepos = initPos 0 2 0
eyedir = ez3
upper = ey3
focus = 1.0 :: Double
xres = 256 :: Int
yres = 256 :: Int

stepx = 2.0 / fromIntegral xres :: Double
stepy = 2.0 / fromIntegral yres :: Double
eex = ex3
eey = negate ey3
origin = focus *> eyedir
  + ((-1.0 + 0.5 * stepx) *> eex)
  - (( 1.0 - 0.5 * stepy) *> eey)

scrmap = [(y, x) | y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  

-- TYPES --

data Radiance = Radiance Double Double Double deriving (Read, Show)

data Wavelength = Rad | Green | Blue deriving (Show, Read, Enum, Eq)
type PhotonCache = (Wavelength, Ray)
data PhotonInfo = PhotonInfo Wavelength Position3 Direction3
  deriving (Show, Eq)

-- FUNCTIONS --

main :: IO ()
main = do
  (power, photonmap) <- readMap
  let image = traceScreen scrmap
  outputImage image

readMap :: IO (Double, KdTree Double PhotonInfo)
readMap = do
  np' <- getLine
  pw' <- getLine
  let np = read np' :: Int
  let pw = read pw' :: Double
  pcs <- forM ([1..np]) $ \i -> do
    l <- getLine
    return $ (read l :: PhotonCache)
  let pmap = build infoToPointList (map convertToInfo pcs)
  return (pw, pmap)

infoToPointList :: PhotonInfo -> [Double]
infoToPointList (PhotonInfo _ (Vector3 x y z) _) = [x, y, z]

convertToInfo :: PhotonCache -> PhotonInfo
convertToInfo (wl, (rp, rd)) = PhotonInfo wl rp (negate rd)

--

traceScreen :: [(Int, Int)] -> [Radiance]
traceScreen [] = []
traceScreen (p:ps) = (Radiance 0.05 0.05 0.05):(traceScreen ps)
--
outputImage :: [Radiance] -> IO ()
outputImage rs = do
  putStrLn "P3"
  putStrLn "## test"
  putStrLn (show xres ++ " " ++ show yres)
  putStrLn "255"
  forM_ rs $ \i -> do
    putStrLn $ convertOneCell i

convertOneCell :: Radiance -> [Char]
convertOneCell (Radiance r g b) =
  (show $ radianceToRgb r) ++ " " ++
  (show $ radianceToRgb g) ++ " " ++
  (show $ radianceToRgb b)

clip = 0.1 :: Double
gamma = 1.0 / 2.2
rgbmax = 255.0

radianceToRgb :: Double -> Int
radianceToRgb d = floor (r * rgbmax)
  where
    d' = d / clip
    r  = (if d' > 1.0 then 1.0 else d') ** gamma
