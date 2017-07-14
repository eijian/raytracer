{-# LANGUAGE NoImplicitPrelude #-}

--
-- Screen module
--

module Screen (
  generateRay'
, useClassicForDirect
, nPhoton
, amb
, radius2
, createHeader
, scrmap
, eyepos
, eyedir
, focus
, xres
, antiAliasing
, diffAliasing
, radianceToRgb
, rgbToString
) where

import Control.Monad
import Data.Maybe
import qualified Data.Vector as V
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Optics

-- PARAMETERS --

-- for rendering

useClassicForDirect :: Bool
--useClassicForDirect = False
useClassicForDirect = True

nPhoton :: Int
nPhoton = 200

amb :: Radiance
amb = Radiance 0.001 0.001 0.001
--amb = Radiance 0.00 0.00 0.00

-- radius for estimation of radiance
radius2 :: Double
--radius2 = 0.0
radius2 = 0.2 * 0.2



-- for camera

eyepos :: Position3
eyepos = initPos 0 2 (-4.5)

targetPos :: Position3
targetPos = initPos 0 2 0

eyedir :: Direction3
eyedir = fromJust $ normalize (targetPos - eyepos)

upper :: Vector3
upper = ey3

focus :: Double
focus = 2.7

xres :: Int
xres = 256

yres :: Int
yres = 256

-- for image output

maxRadiance :: Double
maxRadiance = 0.005

diffAliasing :: Int
diffAliasing = 20

gamma :: Double
gamma = 1.0 / 2.2

rgbmax :: Double
rgbmax = 255.0

antiAliasing :: Bool
--antiAliasing = False
antiAliasing = True

-- CONSTANTS --

stepx :: Double
stepx = 2.0 / fromIntegral xres

stepy :: Double
stepy = 2.0 / fromIntegral yres

step :: (Double, Double)
step = (stepx, stepy)

eex :: Vector3
eex = fromJust $ normalize (upper <*> eyedir)

eey :: Vector3
eey = fromJust $ normalize (eex <*> eyedir)

evec :: (Vector3, Vector3)
evec = (eex, eey)

origin :: Position3
origin = eyepos + focus *> eyedir
  + ((-1.0 + 0.5 * stepx) *> eex)
  - (( 1.0 - 0.5 * stepy) *> eey)

scrmap :: V.Vector (Double, Double)
scrmap = V.fromList [(fromIntegral y, fromIntegral x) |
  y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  

-- FUNCTIONS --

generateRay :: Position3 -> Position3 -> (Double, Double)
            -> (Direction3, Direction3) -> (Double, Double) -> Ray
generateRay e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    tgt   = o + ((sx * x) *> ex) + ((sy * y) *> ey)
    edir  = tgt - e 
    edir' = fromJust $ normalize edir

generateRay' = generateRay eyepos origin step evec

{-
convertToPixels :: V.Vector Radiance -> V.Vector [Int]
convertToPixels rs = V.map toRgb rs
  where
    toRgb :: Radiance -> [Int]
    toRgb (Radiance r g b) = [radianceToRgb clip r
                            , radianceToRgb clip g
                            , radianceToRgb clip b]
-}

createHeader :: [String]
createHeader =
  ["P3"
  ,"## test"
  ,show xres ++ " " ++ show yres
  ,"255"
  ]

radianceToRgb :: Radiance -> (Int, Int, Int)
radianceToRgb (Radiance r g b) =
  ( clip maxRadiance r
  , clip maxRadiance g
  , clip maxRadiance b
  )

clip :: Double -> Double -> Int
clip c d = floor (r * rgbmax)
  where
    d' = d / c
    r  = (if d' > 1.0 then 1.0 else d') ** gamma

rgbToString :: (Int, Int, Int) -> String
rgbToString (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b
