{-# LANGUAGE NoImplicitPrelude #-}

--
-- Screen module
--

module Screen (
  PhotonFilter(..)
, Rgb
, Screen(..)
, readScreen
, rgbToString
) where

--import          Control.Monad
import           Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Vector     as V
import           NumericPrelude

import           Ray.Algebra
import           Ray.Geometry
import           Ray.Optics

import           Parser

--
-- TYPES
--

type Rgb = (Int, Int, Int)

data Screen = Screen
  { xreso     :: Int
  , yreso     :: Int
  , antialias :: Bool
  , nSamplePhoton :: Int
  , useClassicForDirect :: Bool
  , radius2 :: Double
  , pfilter   :: PhotonFilter
  , ambient :: Radiance
  , eyePos        :: Position3
  , eyeDir        :: Direction3
  , focus         :: Double
  , screenMap     :: V.Vector (Double, Double)
  , pnmHeader     :: [String]
  , radianceToRgb :: Radiance -> Rgb
  , generateRay   :: (Double, Double) -> Ray
  }

data PhotonFilter = Nonfilter
                  | Conefilter
                  | Gaussfilter
                  deriving (Eq, Ord, Show, Read)

--
-- CONSTANTS
--

gamma :: Double
gamma = 1.0 / 2.2

rgbmax :: Double
rgbmax = 255.0

defconf :: M.Map String String
defconf = M.fromList [
    (rXresolution   , "512")
  , (rYresolution   , "512")
  , (rAntialias     , "True")
  , (rSamplePhoton  , "100")
  , (rUseClassic    , "True")
  , (rEstimateRadius, "0.2")
  , (rAmbient       , "Radiance 0.001 0.001 0.001")
  , (rMaxRadiance   , "0.01")
  , (rEyePosition   , "Vector3 0.0 2.0 (-4.5)")
  , (rTargetPosition, "Vector3 0.0 2.0 0.0")
  , (rUpperDirection, "Vector3 0.0 1.0 0.0")
  , (rFocus         , "2.7")
  , (rPhotonFilter  , "Nonfilter")
  ]

--
-- PUBLIC FUNCTIONS
--

readScreen :: String -> IO Screen
readScreen file = return scr
  where
    -- input params
    xres       = read (defconf M.! rXresolution   ) :: Int
    yres       = read (defconf M.! rYresolution   ) :: Int
    antialias  = read (defconf M.! rAntialias     ) :: Bool
    samphoton  = read (defconf M.! rSamplePhoton  ) :: Int
    useclassic = read (defconf M.! rUseClassic    ) :: Bool
    radius     = read (defconf M.! rEstimateRadius) :: Double
    amb        = read (defconf M.! rAmbient       ) :: Radiance
    maxrad     = read (defconf M.! rMaxRadiance   ) :: Double
    eyepos     = read (defconf M.! rEyePosition   ) :: Vector3
    targetpos  = read (defconf M.! rTargetPosition) :: Vector3
    upper      = read (defconf M.! rUpperDirection) :: Vector3
    focus      = read (defconf M.! rFocus         ) :: Double
    pfilt      = read (defconf M.! rPhotonFilter  ) :: PhotonFilter

    fmaxrad = radianceToRgb0 maxrad
    fheader = pnmHeader0 xres yres
    smap = V.fromList [(fromIntegral y, fromIntegral x) |
      y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  
    eyedir = fromJust $ normalize (targetpos - eyepos)
    fgenray = makeGenerateRay eyepos eyedir xres yres upper focus
    r2 = radius * radius
    scr = Screen
      xres
      yres
      antialias    -- anti aliasing on/off
      samphoton    -- nSamplePhoton
      useclassic   -- useClassicForDirect
      r2           -- radius for estimation of radiance
      pfilt        -- filter for photon gathering
      amb          -- ambient radiance
      eyepos
      eyedir
      focus
      smap         -- screen map
      fheader      -- func to generate header of PNM format
      fmaxrad      -- func to convert from radiance to rgb
      fgenray      -- func to generate rays

rgbToString :: Rgb -> String
rgbToString (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

--
-- PRIVATE FUNCTIONS
--

makeGenerateRay :: Position3 -> Direction3 -> Int -> Int -> Direction3
                -> Double -> ((Double, Double) -> Ray)
makeGenerateRay epos edir xr yr udir fc = generateRay0 epos origin step evec
  where
    stepx  = 2.0 / fromIntegral xr
    stepy  = 2.0 / fromIntegral yr
    step   = (stepx, stepy)
    eex    = fromJust $ normalize (udir <*> edir)
    eey    = fromJust $ normalize (eex  <*> edir)
    evec   = (eex, eey)
    origin = (target fc (initRay epos edir))
      + ((-1.0 + 0.5 * stepx) *> eex)
      - (( 1.0 - 0.5 * stepy) *> eey)

generateRay0 :: Position3 -> Position3 -> (Double, Double)
             -> (Direction3, Direction3) -> (Double, Double) -> Ray
generateRay0 e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    tgt   = o + ((sx * x) *> ex) + ((sy * y) *> ey)
    edir  = tgt - e 
    edir' = fromJust $ normalize edir

pnmHeader0 :: Int -> Int -> [String]
pnmHeader0 xr yr =
  ["P3"
  ,"## test"
  ,show xr ++ " " ++ show yr
  ,"255"
  ]

radianceToRgb0 :: Double -> Radiance -> Rgb
radianceToRgb0 maxrad (Radiance r g b) =
  ( clip maxrad r
  , clip maxrad g
  , clip maxrad b
  )
  where
    clip :: Double -> Double -> Int
    clip c d = floor (r' * rgbmax)
      where
        d' = d / c
        r'  = (if d' > 1.0 then 1.0 else d') ** gamma
