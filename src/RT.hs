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
step = (stepx, stepy)
eex = ex3
eey = negate ey3
evec = (eex, eey)
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
  let image = map (traceScreen power photonmap) scrmap
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

generateRay' = generateRay eyepos origin step evec
traceScreen :: Double -> KdTree Double PhotonInfo -> (Int, Int) -> Radiance
traceScreen pw pmap cell = traceRay 0 pw pmap objs (generateRay' cell)
--traceScreen (y, x) = Radiance 0.05 0.05 0.05

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
  (show $ radianceToRgb clip r) ++ " " ++
  (show $ radianceToRgb clip g) ++ " " ++
  (show $ radianceToRgb clip b)

gamma = 1.0 / 2.2
rgbmax = 255.0

radianceToRgb :: Double -> Int
radianceToRgb d = floor (r * rgbmax)
  where
    d' = d / clip
    r  = (if d' > 1.0 then 1.0 else d') ** gamma
