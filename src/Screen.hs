{-# LANGUAGE NoImplicitPrelude #-}

--
-- Screen module
--

module Screen (
  Rgb
, Screen(..)
, readScreen
, rgbToString
, rgbToText
, radianceToString
, radianceToText
, rgbToRadiance
) where

--import          Control.Monad
import           Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector as V
import           NumericPrelude
import qualified System.Random.Mersenne as MT

import           Ray.Algebra
import           Ray.Geometry
import           Ray.Optics

import           Parser

--
-- TYPES
--

type Rgb = (Int, Int, Int)

data Screen = Screen
  { nphoton             :: Int
  , progressive         :: Bool
  , xreso               :: Int
  , yreso               :: Int
  , antialias           :: Bool
  , nSamplePhoton       :: Int
  , useClassicForDirect :: Bool
  , radius              :: Double
  , pfilter             :: PhotonFilter
  , ambient             :: Radiance
  , maxradiance         :: Double
  , eyePos              :: Position3
  , eyeDir              :: Direction3
  , focus               :: Double
  , screenMap           :: V.Vector (Double, Double)
  , pnmHeader           :: [String]
  , radianceToRgb       :: Radiance -> Rgb
  , generateRay         :: (Double, Double) -> IO Ray
  }

--
-- CONSTANTS
--

gamma :: Double
gamma = 1.0 / 2.2

rgbmax :: Double
rgbmax = 255.0

defconf :: M.Map String String
defconf = M.fromList [
    (rNPhoton       , "100000")
  , (rProgressive   , "False")
  , (rXresolution   , "256")
  , (rYresolution   , "256")
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
readScreen file = do
  lines <- readConfig file
  let
    -- input params
    conf = parseConfig defconf lines
    --conf = defconf
    nphoton     = read (conf M.! rNPhoton       ) :: Int
    progressive = read (conf M.! rProgressive   ) :: Bool
    xres        = read (conf M.! rXresolution   ) :: Int
    yres        = read (conf M.! rYresolution   ) :: Int
    antialias   = read (conf M.! rAntialias     ) :: Bool
    samphoton   = read (conf M.! rSamplePhoton  ) :: Int
    useclassic  = read (conf M.! rUseClassic    ) :: Bool
    radius      = read (conf M.! rEstimateRadius) :: Double
    amb         = read (conf M.! rAmbient       ) :: Radiance
    maxrad      = read (conf M.! rMaxRadiance   ) :: Double
    eyepos      = read (conf M.! rEyePosition   ) :: Vector3
    targetpos   = read (conf M.! rTargetPosition) :: Vector3
    upper       = read (conf M.! rUpperDirection) :: Vector3
    focus       = read (conf M.! rFocus         ) :: Double
    pfilt       = read (conf M.! rPhotonFilter  ) :: PhotonFilter

    focallen = 50.0 / 1000 :: Double  -- 焦点距離 50mm
    fnumber  = 5.0 :: Double          -- F/5.0

    fmaxrad = radianceToRgb0 maxrad
    fheader = pnmHeader0 xres yres maxrad
    smap = V.fromList [(fromIntegral y, fromIntegral x) |
      y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  
    eyedir = fromJust $ normalize (targetpos - eyepos)
    fgenray = makeGenerateRay antialias progressive eyepos targetpos xres yres upper focus focallen fnumber
    radius2 = radius * radius
    scr = Screen
      nphoton
      progressive
      xres
      yres
      antialias    -- anti aliasing on/off
      samphoton    -- nSamplePhoton
      useclassic   -- useClassicForDirect
      radius2      -- radius for radiance estimate
      pfilt        -- filter for photon gathering
      amb          -- ambient radiance
      maxrad
      eyepos
      eyedir
      focus
      smap         -- screen map
      fheader      -- func to generate header of PNM format
      fmaxrad      -- func to convert from radiance to rgb
      fgenray      -- func to generate rays
  return scr

rgbToString :: Rgb -> String
rgbToString (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

rgbToText :: Rgb -> T.Text
rgbToText (r, g, b) = T.pack (show r ++ " " ++ show g ++ " " ++ show b)

radianceToString :: Radiance -> String
radianceToString (Radiance r g b) = show r ++ " " ++ show g ++ " " ++ show b

radianceToText :: Radiance -> T.Text
radianceToText (Radiance r g b) = T.pack (show r ++ " " ++ show g ++ " " ++ show b)

rgbToRadiance :: Screen -> Rgb -> Radiance
rgbToRadiance scr (r, g, b) =
  Radiance (fromIntegral r * mag)
           (fromIntegral g * mag)
           (fromIntegral b * mag)
  where
    mag = maxradiance scr / rgbmax
--
-- PRIVATE FUNCTIONS
--

readConfig :: String -> IO [String]
readConfig file = do
  f <- readFile file
  return $ map removeComment $ lines f

parseConfig :: M.Map String String -> [String] -> M.Map String String
parseConfig c ls = updateConfig c ps
  where
    ps = parseLines ls
    updateConfig :: M.Map String String -> [Param] -> M.Map String String
    updateConfig c []     = c
    updateConfig c ((k,v):ps) = updateConfig c' ps
      where
        c' = if k /= "" then M.insert k v c else c

parseLines :: [String] -> [Param]
parseLines []     = []
parseLines (l:ls) = p:(parseLines ls)
  where
    p = case (parse sline "rt screen file parse error" l) of
        Left  e  -> error $ (show e ++ "\nLINE: " ++ l)
        Right p' -> p'

makeGenerateRay :: Bool -> Bool
                -> Position3 -> Direction3 -> Int -> Int -> Direction3
                -> Double -> Double -> Double
                -> ((Double, Double) -> IO Ray)
makeGenerateRay aaflag prflag epos target xr yr udir fd fl = do
  let
    ez = fromJust $ normalize (target - epos)
    ex = fromJust $ normalize (udir <*> ez)
    ey = fromJust $ normalize (ex   <*> ez)
    --step = 2.0 * (fd / fl) / fromIntegral xr
    step = (fd * 0.035 / fl)/ fromIntegral xr
    esx  = step *> ex
    esy  = step *> ey
    fnum = 4.0 :: Double
    ea   = fl / fnum
    eex  = ea *> ex
    eey  = ea *> ey
    lx = fromIntegral (xr `div` 2) :: Double
    ly = fromIntegral (yr `div` 2) :: Double
    orig = fd *> ez - (lx - 0.5) *> esx - (ly - 0.5) *> esy
    --blurflag = fnum < 1000
    blurflag = False
  
  return (generateRay0 aaflag prflag blurflag epos orig esx esy eex eey)

generateRay0 :: Bool -> Bool -> Bool -> Position3 -> Position3
             -> Direction3 -> Direction3 -> Direction3 -> Direction3
             -> (Double, Double) -> IO Ray
generateRay0 aaflag prflag blurflag e o esx esy eex eey (y, x) = do
  blur <- if blurflag == True
    then do
      r1 <- MT.randomIO :: IO Double
      r2 <- MT.randomIO :: IO Double
      return ((r1 - 0.5) *> eex + (r2 - 0.5) *> eey)
    else
      return o3
  (r3, r4) <- if prflag == True && aaflag == True
    then do
      r3' <- MT.randomIO :: IO Double
      r4' <- MT.randomIO :: IO Double
      return (r3' - 0.5, r4' - 0.5)
    else
      return (0.0, 0.0)
  let
    eyepos = e + blur
    eyedir = o + (x + r3) *> esx + (y + r4) *> esy - blur
  return (initRay eyepos (fromJust $ normalize eyedir))
  
pnmHeader0 :: Int -> Int -> Double -> [String]
pnmHeader0 xr yr maxrad =
  ["P3"
  ,"## max radiance = " ++ show maxrad
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
