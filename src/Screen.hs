{-# LANGUAGE NoImplicitPrelude #-}

--
-- Screen module
--

module Screen (
  Rgb
, useClassicForDirect
, nPhoton
, nSamplePhoton
, radius2
, eyepos
, eyedir
, focus
--, xres
--, yres
, xreso
, yreso
, pfilter
, scrmap
, pnmHeader
, antiAliasing
, generateRay
, radianceToRgb
, readScreen
, rgbToString
, Screen
, ambient
, PhotonFilter(..)
) where

--import Control.Monad
import Data.Maybe
import qualified Data.Vector as V
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Optics

type Rgb = (Int, Int, Int)

data Screen = Screen
  { nPhoton :: Int
  , nSamplePhoton :: Int
  , useClassicForDirect :: Bool
  , radius2 :: Double
  , ambient :: Radiance
  , antialias :: Bool
  , xreso     :: Int
  , yreso     :: Int
  , pfilter   :: PhotonFilter
  }

data PhotonFilter = Nonfilter | Conefilter | Gaussfilter deriving (Eq, Ord)

--
-- CONSTANTS
--

gamma :: Double
gamma = 1.0 / 2.2

rgbmax :: Double
rgbmax = 255.0


--
-- PUBLIC
--

readScreen :: String -> IO Screen
readScreen conf = return scr
  where
    scr = Screen
      100000       -- nPhoton
      100          -- nSamplePhoton
      True         -- useClassicForDirect
      (0.2 * 0.2)  -- radius for estimation of radiance
      amb          -- ambient radiance
      antiAliasing -- anti aliasing on/off
      xres
      yres
      Nonfilter
      

-- for rendering

amb :: Radiance
amb = Radiance 0.001 0.001 0.001
--amb = Radiance 0.00 0.00 0.00

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
--maxRadiance = 0.005
maxRadiance = 0.01

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

center :: Position3
center = target focus (initRay eyepos eyedir)

origin :: Position3
--origin = eyepos + focus *> eyedir
origin = center
  + ((-1.0 + 0.5 * stepx) *> eex)
  - (( 1.0 - 0.5 * stepy) *> eey)

scrmap :: V.Vector (Double, Double)
scrmap = V.fromList [(fromIntegral y, fromIntegral x) |
  y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  

-- FUNCTIONS --

generateRay0 :: Position3 -> Position3 -> (Double, Double)
             -> (Direction3, Direction3) -> (Double, Double) -> Ray
generateRay0 e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    tgt   = o + ((sx * x) *> ex) + ((sy * y) *> ey)
    edir  = tgt - e 
    edir' = fromJust $ normalize edir

generateRay :: (Double, Double) -> Ray
generateRay = generateRay0 eyepos origin step evec

pnmHeader :: [String]
pnmHeader =
  ["P3"
  ,"## test"
  ,show xres ++ " " ++ show yres
  ,"255"
  ]

radianceToRgb :: Radiance -> Rgb
radianceToRgb (Radiance r g b) =
  ( clip maxRadiance r
  , clip maxRadiance g
  , clip maxRadiance b
  )
  where
    clip :: Double -> Double -> Int
    clip c d = floor (r' * rgbmax)
      where
        d' = d / c
        r'  = (if d' > 1.0 then 1.0 else d') ** gamma

rgbToString :: Rgb -> String
rgbToString (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b
