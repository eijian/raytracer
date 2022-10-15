{-# LANGUAGE NoImplicitPrelude #-}

--
-- Camera module
--

module Camera (
  Rgb
, Camera(..)
, generateRay
, radianceToString
, radianceToText
, readCamera
--, rgbToRadiance
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
  -- camera spacs
  { focalLength         :: Double   -- 焦点距離(mm)
  , fnumber             :: Double   -- F値
  , focusDistance       :: Double   -- フォーカス距離(m)
  , isoSensitivity      :: Double   -- ISO感度
  , shutterSpeed        :: Double   -- シャッタースピード(/sec)
  , lightAmount         :: Double   -- 光量
  , lens                :: (Direction3, Direction3)   -- レンズXYベクトル
  , blurFlag           :: Bool     -- ボケ表現ON/OFF

  -- screen quality
  , resolution          :: (Int, Int)  -- 画面解像度
  , antialias           :: Bool        -- アンチエリアスON/OFF
  --, nSamplePhoton       :: Int
  --, mapDivision         :: Int
  , nphoton             :: Int                         -- フォトン数
  , radius              :: Double                      -- フォトン収集半径
  , pfilter             :: PhotonFilter                -- 収集時フィルター
  --, ambient             :: Radiance   
  --, maxradiance         :: Double
  -- visual environment
  , eyePos              :: Position3                   -- 視点
  , eyeDir              :: Direction3                  -- 視線
  --, focus               :: Double
  , screenOrigin        :: Position3                   -- スクリーン開始点
  , screenStep          :: (Direction3, Direction3)    -- スクリーン移動量
  , screenMap           :: V.Vector (Double, Double)   -- スクリーン位置集合
  -- for output
  , pnmHeader           :: [String]                    -- PNMファイルヘッダ
  --, radianceToRgb       :: Radiance -> Rgb             -- 輝度-RGB変換関数
  --, generateRay         :: (Double, Double) -> IO Ray
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
    (rFocalLength   , "50")
  , (rFnumber       , "100")        -- ボケ味なし
  , (rFocusDistance , "2.0")
  , (rIsoSensitivity, "100")
  , (rShutterSpeed  , "50")
  , (rXresolution   , "256")
  , (rYresolution   , "256")
  , (rAntialias     , "True")
  , (rNPhoton       , "100000")
  , (rEstimateRadius, "0.2")
  , (rPhotonFilter  , "Nonfilter")
  , (rEyePosition   , "Vector3 0.0 2.0 (-4.5)")
  , (rTargetPosition, "Vector3 0.0 2.0 0.0")
  , (rUpperDirection, "Vector3 0.0 1.0 0.0")
--  , (rProgressive   , "False")
--  , (rSamplePhoton  , "100")
--  , (rMapDivision   , "1")
--  , (rAmbient       , "Radiance 0.001 0.001 0.001")
--  , (rMaxRadiance   , "0.01")
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
    focallen0   = read (conf M.! rFocalLength   ) :: Double
    fnumber     = read (conf M.! rFnumber       ) :: Double
    focusdist   = read (conf M.! rFocusDistance ) :: Double
    isosens     = read (conf M.! rIsoSensitivity) :: Double
    shutterspd  = read (conf M.! rShutterSpeed  ) :: Double
    xres        = read (conf M.! rXresolution   ) :: Int
    yres        = read (conf M.! rYresolution   ) :: Int
    antialias   = read (conf M.! rAntialias     ) :: Bool
    nphoton     = read (conf M.! rNPhoton       ) :: Int
    radius      = read (conf M.! rEstimateRadius) :: Double
    pfilt       = read (conf M.! rPhotonFilter  ) :: PhotonFilter
    eyepos      = read (conf M.! rEyePosition   ) :: Vector3
    targetpos   = read (conf M.! rTargetPosition) :: Vector3
    upper       = read (conf M.! rUpperDirection) :: Vector3
    --progressive = read (conf M.! rProgressive   ) :: Bool
    --samphoton   = read (conf M.! rSamplePhoton  ) :: Int
    --mapdivision = read (conf M.! rMapDivision   ) :: Int
    --amb         = read (conf M.! rAmbient       ) :: Radiance
    --maxrad      = read (conf M.! rMaxRadiance   ) :: Double
    --focus       = read (conf M.! rFocus         ) :: Double

    focallen = focallen0 / 1000.0 :: Double
    --fnumber  = 5.0 :: Double          -- F/5.0
    lightamount = (isosens / 100.0) / (shutterspd / 50.0) / (focallen0 / 50.0)

    --fmaxrad = radianceToRgb0 maxrad
    fheader = pnmHeader0 xres yres 1.0
    smap = V.fromList [(fromIntegral y, fromIntegral x) |
      y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  
    eyedir = fromJust $ normalize (targetpos - eyepos)
    --fgenray = makeGenerateRay antialias eyepos targetpos xres yres upper focus focallen fnumber
    radius2 = radius * radius -- square of radius
 
    ez = eyedir
    ex = fromJust $ normalize (upper <*> ez)
    ey = fromJust $ normalize (ex    <*> ez)
    --step = 2.0 * (fd / fl) / fromIntegral xr
    step = (focusdist * 0.035 / focallen)/ fromIntegral xres  -- 35mm換算
    esx  = step *> ex
    esy  = step *> ey
    ea   = focallen / fnumber / 2.0
    lens_x  = ea *> ex
    lens_y  = ea *> ey
    lx = fromIntegral (xres `div` 2) :: Double
    ly = fromIntegral (yres `div` 2) :: Double
    orig = focusdist *> ez - (lx - 0.5) *> esx - (ly - 0.5) *> esy
    --blurflag = fnum < 1000
    blurflag = if fnumber >= 100 then False else True  -- F値が100以上ならピンホールカメラ

 
 
 
    cam = Camera
      focallen
      fnumber
      focusdist
      isosens
      shutterspd
      lightamount
      (lens_x, lens_y)
      blurflag
      (xres, yres)
      antialias    -- anti aliasing on/off
      nphoton
      radius2      -- radius for radiance estimate
      pfilt        -- filter for photon gathering
      eyepos
      eyedir
      orig
      (esx, esy)
      smap
      fheader
    {-
      --progressive
      xres
      yres
      samphoton    -- nSamplePhoton
      --mapdivision  -- photon map division number
      amb          -- ambient radiance
      maxrad
      eyedir
      focus
      smap         -- screen map
      fheader      -- func to generate header of PNM format
      fmaxrad      -- func to convert from radiance to rgb
      fgenray      -- func to generate rays
    -}
  return cam

rgbToString :: Rgb -> String
rgbToString (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

rgbToText :: Rgb -> T.Text
rgbToText (r, g, b) = T.pack (show r ++ " " ++ show g ++ " " ++ show b)

radianceToString :: Radiance -> String
radianceToString (Radiance r g b) = show r ++ " " ++ show g ++ " " ++ show b

radianceToText :: Radiance -> T.Text
radianceToText (Radiance r g b) = T.pack (show r ++ " " ++ show g ++ " " ++ show b)

{-
rgbToRadiance :: Camera -> Rgb -> Radiance
rgbToRadiance cam (r, g, b) =
  Radiance (fromIntegral r * mag)
           (fromIntegral g * mag)
           (fromIntegral b * mag)
  where
    mag = maxradiance cam / rgbmax
-}
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

{-
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
-}

generateRay :: Camera -> (Double, Double) -> IO Ray

--Bool -> Bool -> Position3 -> Position3
--             -> Direction3 -> Direction3 -> Direction3 -> Direction3
--             -> (Double, Double) -> IO Ray
generateRay cam (y, x) = do
  --aaflag blurflag eyepos0 origin esx esy lens_x lens_y (y, x) = do
  blur <- lensOffset (blurFlag cam) (lens cam)
  --putStrLn ("OR:" ++ show origin ++ ", BL:" ++ show blur)
  --putStrLn ("BL:" ++ show blur)
  (r3, r4) <- if (antialias cam) == True
    then do
      r3' <- MT.randomIO :: IO Double
      r4' <- MT.randomIO :: IO Double
      return (r3' - 0.5, r4' - 0.5)
    else
      return (0.0, 0.0)
  let
    eyepos = (eyePos cam) + blur
    (esx, esy) = screenStep cam
    eyedir = (screenOrigin cam) + (x + r3) *> esx + (y + r4) *> esy - blur
  return (initRay eyepos (fromJust $ normalize eyedir))

lensOffset :: Bool -> (Direction3, Direction3) -> IO Direction3
lensOffset blurflag (lens_x, lens_y) = if blurflag == False
    then return o3
    else do
      r1 <- MT.randomIO :: IO Double
      r2 <- MT.randomIO :: IO Double
      let
        ofx = 2.0 * (r1 - 0.5)
        ofy = 2.0 * (r2 - 0.5)
      if ofx * ofx + ofy * ofy <= 1.0
        then return (ofx *> lens_x + ofy *> lens_y)
        else lensOffset blurflag (lens_x, lens_y)

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
