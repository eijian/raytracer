{-# LANGUAGE NoImplicitPrelude #-}

--
-- Camera module
--

module Camera (
  Rgb
, Camera(..)
, compensateExposure
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
  , exposure            :: Double   -- 光量
  , lens                :: (Direction3, Direction3)   -- レンズXYベクトル
  , blurFlag           :: Bool     -- ボケ表現ON/OFF

  -- screen quality
  , resolution          :: (Int, Int)  -- 画面解像度
  , antialias           :: Bool        -- アンチエリアスON/OFF
  , nphoton             :: Int                         -- フォトン数
  , radius              :: Double                      -- フォトン収集半径
  , pfilter             :: PhotonFilter                -- 収集時フィルター
  -- visual environment
  , eyePos              :: Position3                   -- 視点
  , eyeDir              :: Direction3                  -- 視線
  , screenOrigin        :: Position3                   -- スクリーン開始点
  , screenStep          :: (Direction3, Direction3)    -- スクリーン移動量
  , screenMap           :: V.Vector (Double, Double)   -- スクリーン位置集合
  -- for output
  , pnmHeader           :: [String]                    -- PNMファイルヘッダ
  , ambient             :: Radiance   
  }

--
-- CONSTANTS
--

gamma :: Double
gamma = 1.0 / 2.2

rgbmax :: Double
rgbmax = 255.0

exposureK :: Double
exposureK = 5.0

exposureMag :: Double
exposureMag = 100.0

defconf :: M.Map String String
defconf = M.fromList [
    (rFocalLength   , "50")
  , (rFnumber       , "100")        -- ボケ味なし
  , (rFocusDistance , "2.0")
  , (rIsoSensitivity, "100")
  , (rShutterSpeed  , "50")
  , (rResolution   , "(256, 256)")
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
  , (rAmbient       , "Radiance 0.001 0.001 0.001")
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
    focallen0   = read (conf M.! rFocalLength   ) :: Double
    fnumber     = read (conf M.! rFnumber       ) :: Double
    focusdist   = read (conf M.! rFocusDistance ) :: Double
    isosens     = read (conf M.! rIsoSensitivity) :: Double
    shutterspd  = read (conf M.! rShutterSpeed  ) :: Double
    reso        = read (conf M.! rResolution    ) :: (Int, Int)
    antialias   = read (conf M.! rAntialias     ) :: Bool
    nphoton     = read (conf M.! rNPhoton       ) :: Int
    radius      = read (conf M.! rEstimateRadius) :: Double
    pfilt       = read (conf M.! rPhotonFilter  ) :: PhotonFilter
    eyepos      = read (conf M.! rEyePosition   ) :: Vector3
    targetpos   = read (conf M.! rTargetPosition) :: Vector3
    upper       = read (conf M.! rUpperDirection) :: Vector3
    amb         = read (conf M.! rAmbient       ) :: Radiance

    focallen = focallen0 / 1000.0 :: Double
    ea   = focallen / fnumber / 2.0
    -- 露出係数の決め方
    -- 参考: https://www.ccs-inc.co.jp/guide/column/light_color/vol26.html
    exposure0 = exposureK * exposureMag * isosens / shutterspd / (fnumber ** 2) 
    ez = fromJust $ normalize (targetpos - eyepos)
    ex = fromJust $ normalize (upper <*> ez)
    ey = fromJust $ normalize (ex    <*> ez)
    (xr, yr) = reso
    step = (focusdist * 0.035 / focallen)/ fromIntegral xr  -- 35mm換算
    hr_x = fromIntegral (xr `div` 2) :: Double -- half resolution
    hr_y = fromIntegral (yr `div` 2) :: Double
 
    exposure = if fnumber >= 100 then 100.0 else exposure0
    lens_x  = ea *> ex
    lens_y  = ea *> ey
    blurflag = if fnumber >= exposureMag then False else True  -- F値が100以上ならピンホールカメラ
    radius2 = radius * radius -- square of radius
    eyedir = ez
    orig = focusdist *> ez - (hr_x - 0.5) *> step_x - (hr_y - 0.5) *> step_y
    step_x  = step *> ex
    step_y  = step *> ey
    smap = V.fromList [(fromIntegral y, fromIntegral x) |
      y <- [0..(yr - 1)], x <- [0..(xr - 1)]]  
    fheader = pnmHeader0 reso 0.001

    cam = Camera
      focallen
      fnumber
      focusdist
      isosens
      shutterspd
      exposure
      (lens_x, lens_y)
      blurflag
      reso
      antialias    -- anti aliasing on/off
      nphoton
      radius2      -- radius for radiance estimate
      pfilt        -- filter for photon gathering
      eyepos
      eyedir
      orig
      (step_x, step_y)
      smap
      fheader
      amb

  --putStrLn ("EXP:" ++ show lightamount)
  return cam

compensateExposure :: Camera -> Radiance -> Radiance
compensateExposure cam rad = (exposure cam) *> rad

rgbToString :: Rgb -> String
rgbToString (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

rgbToText :: Rgb -> T.Text
rgbToText (r, g, b) = T.pack (show r ++ " " ++ show g ++ " " ++ show b)

radianceToString :: Radiance -> String
radianceToString (Radiance r g b) = show r ++ " " ++ show g ++ " " ++ show b

radianceToText :: Radiance -> T.Text
radianceToText (Radiance r g b) = T.pack (show r ++ " " ++ show g ++ " " ++ show b)

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

generateRay :: Camera -> (Double, Double) -> IO Ray
generateRay cam (y, x) = do
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

pnmHeader0 :: (Int, Int) -> Double -> [String]
pnmHeader0 (xr, yr) maxrad =
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
