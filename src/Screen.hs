{-# LANGUAGE NoImplicitPrelude #-}

--
-- Screen module
--

module Screen (
  generateRay'
, outputImage
, scrmap
, eyepos
, eyedir
, focus
) where

import System.IO
import Control.Monad
import Data.Maybe
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
scrmap = [(y, x) | y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  

-- FUNCTIONS --

generateRay :: Position3 -> Position3 -> (Double, Double)
            -> (Direction3, Direction3) -> (Int, Int) -> Ray
generateRay e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    tgt   = o + ((sx * fromIntegral x) *> ex) + ((sy * fromIntegral y) *> ey)
    edir  = tgt - e 
    edir' = fromJust $ normalize edir

generateRay' = generateRay eyepos origin step evec

outputImage :: [Radiance] -> IO ()
outputImage rs = do
{-
  putStrLn "P3"
  putStrLn "## test"
  putStrLn (show xres ++ " " ++ show yres)
  putStrLn "255"
-}
  mapM putStrLn $ createHeader xres yres
  forM_ rs $ \i -> do
    putStrLn $ convertOneCell i

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
