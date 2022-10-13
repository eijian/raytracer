{-# LANGUAGE NoImplicitPrelude #-}

--
-- Camera module
--

module Camera (
  Rgb
, Camera(..)
, radianceToString
, radianceToText
, readCamera
, rgbToRadiance
, rgbToString
, rgbToText
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

data Camera = Camera
  { nphoton             :: Int
  --, progressive         :: Bool
  , xreso               :: Int
  , yreso               :: Int
  , antialias           :: Bool
  , nSamplePhoton       :: Int
  --, mapDivision         :: Int
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
--  , (rProgressive   , "False")
  , (rXresolution   , "256")
  , (rYresolution   , "256")
  , (rAntialias     , "True")
  , (rSamplePhoton  , "100")
--  , (rMapDivision   , "1")
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

readCamera :: String -> IO Camera
readCamera file = do
  lines <- readConfig file
  let
    -- input params
    conf = parseConfig defconf lines
    --conf = defconf
    nphoton     = read (conf M.! rNPhoton       ) :: Int
    --progressive = read (conf M.! rProgressive   ) :: Bool
    xres        = read (conf M.! rXresolution   ) :: Int
    yres        = read (conf M.! rYresolution   ) :: Int
    antialias   = read (conf M.! rAntialias     ) :: Bool
    samphoton   = read (conf M.! rSamplePhoton  ) :: Int
    --mapdivision = read (conf M.! rMapDivision   ) :: Int
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
    fgenray = makeGenerateRay antialias eyepos targetpos xres yres upper focus focallen fnumber
    radius2 = radius * radius -- square of radius
    cam = Camera
      nphoton
      --progressive
      xres
      yres
      antialias    -- anti aliasing on/off
      samphoton    -- nSamplePhoton
      --mapdivision  -- photon map division number
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
  return cam

rgbToString :: Rgb -> String
rgbToString (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

rgbToText :: Rgb -> T.Text
rgbToText (r, g, b) = T.pack (show r ++ " " ++ show g ++ " " ++ show b)

radianceToString :: Radiance -> String
radianceToString (Radiance r g b) = show r ++ " " ++ show g ++ " " ++ show b

radianceToText :: Radiance -> T.Text
radianceToText (Radiance r g b) = T.pack (show r ++ " " ++ show g ++ " " ++ show b)

rgbToRadiance :: Camera -> Rgb -> Radiance
rgbToRadiance cam (r, g, b) =
  Radiance (fromIntegral r * mag)
           (fromIntegral g * mag)
           (fromIntegral b * mag)
  where
    mag = maxradiance cam / rgbmax
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
    p = case (parse sline "rt camera file parse error" l) of
        Left  e  -> error $ (show e ++ "\nLINE: " ++ l)
        Right p' -> p'

makeGenerateRay :: Bool -> Position3 -> Direction3 -> Int -> Int -> Direction3
                -> Double -> Double -> Double
                -> ((Double, Double) -> IO Ray)
makeGenerateRay aaflag epos target xr yr udir fd fl = do
  let
    fnum = 1.8
    ez = fromJust $ normalize (target - epos)
    ex = fromJust $ normalize (udir <*> ez)
    ey = fromJust $ normalize (ex   <*> ez)
    --step = 2.0 * (fd / fl) / fromIntegral xr
    step = (fd * 0.035 / fl)/ fromIntegral xr
    esx  = step *> ex
    esy  = step *> ey
    ea   = fl / fnum / 2.0
    lens_x  = ea *> ex
    lens_y  = ea *> ey
    lx = fromIntegral (xr `div` 2) :: Double
    ly = fromIntegral (yr `div` 2) :: Double
    orig = fd *> ez - (lx - 0.5) *> esx - (ly - 0.5) *> esy
    --blurflag = fnum < 1000
    blurflag = if fnum >= 100 then False else True  -- F値が100以上ならピンホールカメラ
    fgenray = generateRay0 aaflag blurflag epos orig esx esy lens_x lens_y
  return fgenray

generateRay0 :: Bool -> Bool -> Position3 -> Position3
             -> Direction3 -> Direction3 -> Direction3 -> Direction3
             -> (Double, Double) -> IO Ray
generateRay0 aaflag blurflag eyepos0 origin esx esy lens_x lens_y (y, x) = do
  blur <- lensOffset blurflag lens_x lens_y
  --putStrLn ("OR:" ++ show origin ++ ", BL:" ++ show blur)
  --putStrLn ("BL:" ++ show blur)
  (r3, r4) <- if aaflag == True
    then do
      r3' <- MT.randomIO :: IO Double
      r4' <- MT.randomIO :: IO Double
      return (r3' - 0.5, r4' - 0.5)
    else
      return (0.0, 0.0)
  let
    eyepos = eyepos0 + blur
    eyedir = origin + (x + r3) *> esx + (y + r4) *> esy - blur
  return (initRay eyepos (fromJust $ normalize eyedir))

lensOffset :: Bool -> Direction3 -> Direction3 -> IO Direction3
lensOffset blurflag lens_x lens_y = if blurflag == False
    then return o3
    else do
      r1 <- MT.randomIO :: IO Double
      r2 <- MT.randomIO :: IO Double
      let
        ofx = 2.0 * (r1 - 0.5)
        ofy = 2.0 * (r2 - 0.5)
      if ofx * ofx + ofy * ofy <= 1.0
        then return (ofx *> lens_x + ofy *> lens_y)
        else lensOffset blurflag lens_x lens_y

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
