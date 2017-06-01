{-# LANGUAGE NoImplicitPrelude #-}

--
-- Screen module
--

module Screen (
  generateRay'
, outputHeader
, outputImage
, yline
, oneLine
, eyepos
, eyedir
, focus
) where

import System.IO
import Control.Monad
import Data.Maybe
import qualified Data.Vector as V
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Optics

-- PARAMETERS --

-- for camera

eyepos :: Position3
eyepos = initPos 0 2 (-4.5)

eyedir :: Direction3
eyedir = ez3

upper :: Vector3
upper = ey3

focus :: Double
focus = 2.7

xres :: Int
xres = 256

yres :: Int
yres = 256

-- for image output

clip :: Double
clip = 0.005

gamma :: Double
gamma = 1.0 / 2.2

rgbmax :: Double
rgbmax = 255.0

-- CONSTANTS --

stepx :: Double
stepx = 2.0 / fromIntegral xres

stepy :: Double
stepy = 2.0 / fromIntegral yres

step :: (Double, Double)
step = (stepx, stepy)

eex :: Vector3
eex = ex3

eey :: Vector3
eey = negate ey3

evec :: (Vector3, Vector3)
evec = (eex, eey)

origin :: Position3
origin = eyepos + focus *> eyedir
  + ((-1.0 + 0.5 * stepx) *> eex)
  - (( 1.0 - 0.5 * stepy) *> eey)


yline :: [Int]
yline = [0..(yres - 1)]

scrmap :: V.Vector (Int, Int)
scrmap = V.fromList [(y, x) | y <- yline, x <- [0..(xres - 1)]]  

-- FUNCTIONS --

oneLine :: Int -> V.Vector (Int, Int)
oneLine y = V.fromList [(y, x) | x <- [0..(xres - 1)]]

generateRay :: Position3 -> Position3 -> (Double, Double)
            -> (Direction3, Direction3) -> (Int, Int) -> Ray
generateRay e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    tgt   = o + ((sx * fromIntegral x) *> ex) + ((sy * fromIntegral y) *> ey)
    edir  = tgt - e 
    edir' = fromJust $ normalize edir

generateRay' = generateRay eyepos origin step evec

outputImage :: V.Vector Radiance -> IO ()
outputImage rs = do
  V.forM_ rs $ \i -> do
    putStrLn $ convertOneCell i

outputHeader :: IO ()
outputHeader = do
  mapM_ putStrLn $ createHeader xres yres

createHeader :: Int -> Int -> [String]
createHeader xres yres =
  ["P3"
  ,"## test"
  ,show xres ++ " " ++ show yres
  ,"255"
  ]

convertOneCell :: Radiance -> String
convertOneCell (Radiance r g b) =
  (show $ radianceToRgb clip r) ++ " " ++
  (show $ radianceToRgb clip g) ++ " " ++
  (show $ radianceToRgb clip b)

radianceToRgb :: Double -> Double -> Int
radianceToRgb c d = floor (r * rgbmax)
  where
    d' = d / c
    r  = (if d' > 1.0 then 1.0 else d') ** gamma
