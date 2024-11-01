{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

--
-- Camera module
--

module Camera (
  Rgb
, Camera (..)
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
  , shutterSpeed        :: Double   -- シャッタースピード(/sec)
  --, isoSensitivity      :: Double   -- ISO感度
  , whiteBalance        :: Double   -- ホワイトバランス(K) ←標準をCANON太陽光(5200K)とした時の差分
  , focusDistance       :: Double   -- フォーカス距離(m)
  , exposure            :: Double   -- 光量（露出）
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
-- CONSTANTS & DEFAULTS
--

--gamma :: Double
--gamma = 1.0 / 2.2

--rgbmax :: Double
--rgbmax = 255.0

-- 標準EV値=14とする（晴天時の屋外相当）
standardEV :: Double
standardEV = 14.0

-- 標準ISO感度=100とする
standardISO :: Double
standardISO = 100.0

-- 撮像素子サイズ 36mm x 24mmとして横幅基準とする
sensorSize :: Double
sensorSize = 36.0 / 1000.0   -- 36mm

-- 標準ホワイトバランス=5200K
-- CANON一眼の"太陽光"
-- https://faq.canon.jp/app/answers/detail/a_id/61330/~/%E3%80%90%E3%83%87%E3%82%B8%E3%82%BF%E3%83%AB%E4%B8%80%E7%9C%BC%E3%83%AC%E3%83%95%E3%82%AB%E3%83%A1%E3%83%A9%E3%83%BB%E3%83%9F%E3%83%A9%E3%83%BC%E3%83%AC%E3%82%B9%E3%82%AB%E3%83%A1%E3%83%A9%E3%80%91%E3%83%9B%E3%83%AF%E3%82%A4%E3%83%88%E3%83%90%E3%83%A9%E3%83%B3%E3%82%B9%E3%81%A8%E3%81%AF%EF%BC%9F)
standardWB :: Double
standardWB = 5200.0

-- カメラパタメータ（デフォルト値）
defconf :: M.Map String String
defconf = M.fromList [
    (rFocalLength   , "50")
  , (rFnumber       , "100")
  , (rShutterSpeed  , "50")
  , (rIsoSensitivity, "100")
  , (rEvAdjustment  , "0.0")
  , (rWhiteBalance  , "5200")
  , (rFocusDistance , "2.0")
  , (rResolution    , "(256, 256)")
  , (rAntialias     , "True")
  , (rNPhoton       , "100000")
  , (rEstimateRadius, "0.2")
  , (rPhotonFilter  , "Nonfilter")
  , (rEyePosition   , "Vector3 0.0 2.0 (-4.5)")
  , (rTargetPosition, "Vector3 0.0 2.0 0.0")
  , (rUpperDirection, "Vector3 0.0 1.0 0.0")
  , (rAmbient       , "Radiance 0.001 0.001 0.001")
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
    shutterspd  = read (conf M.! rShutterSpeed  ) :: Double
    isosens     = read (conf M.! rIsoSensitivity) :: Double
    evadjust    = read (conf M.! rEvAdjustment  ) :: Double
    wb0         = read (conf M.! rWhiteBalance  ) :: Double
    focusdist   = read (conf M.! rFocusDistance ) :: Double
    reso        = read (conf M.! rResolution    ) :: (Int, Int)
    antialias   = read (conf M.! rAntialias     ) :: Bool
    nphoton     = read (conf M.! rNPhoton       ) :: Int
    radius      = read (conf M.! rEstimateRadius) :: Double
    pfilt       = read (conf M.! rPhotonFilter  ) :: PhotonFilter
    eyepos      = read (conf M.! rEyePosition   ) :: Vector3
    targetpos   = read (conf M.! rTargetPosition) :: Vector3
    upper       = read (conf M.! rUpperDirection) :: Vector3
    amb         = read (conf M.! rAmbient       ) :: Radiance

    focallen = focallen0 / 1000.0   -- 焦点距離を[m]単位に変換
    ea = focallen / fnumber / 2.0   -- 開放口径（の半径）

    ez = fromJust $ normalize (targetpos - eyepos)
    ex = fromJust $ normalize (upper <*> ez)
    ey = fromJust $ normalize (ex    <*> ez)
    (xr, yr) = reso
    step = (focusdist * sensorSize / focallen) / fromIntegral xr
    hr_x = fromIntegral (xr `div` 2) :: Double -- half resolution
    hr_y = fromIntegral (yr `div` 2) :: Double
 
    -- 露出係数の決め方
    -- 参考: https://www.ccs-inc.co.jp/guide/column/light_color/vol26.html
    --ev = (logBase 2 (fnumber ** 2)) + (logBase 2 shutterspd) - evadjust
    ev = logBase (sqrt 2) fnumber + logBase 2 shutterspd - evadjust
    exposure = 2.0 ** (standardEV - ev) * (isosens / standardISO)
    
    wb = standardWB - wb0

    lens_x  = ea *> ex
    lens_y  = ea *> ey
    blurflag = fnumber < 100  -- F値が100未満なら通常、100以上ならピンホールカメラとする
    radius2 = radius * radius -- square of radius
    eyedir = ez
    orig = focusdist *> ez - (hr_x - 0.5) *> step_x - (hr_y - 0.5) *> step_y
    step_x  = step *> ex
    step_y  = step *> ey
    smap = V.fromList [(fromIntegral y, fromIntegral x) |
      y <- [0..(yr - 1)], x <- [0..(xr - 1)]]  
    --fheader = pnmHeader0 reso 0.001
    fheader = pnmHeader0 reso 1.0

    cam = Camera
      focallen
      fnumber
      shutterspd
      wb
      focusdist
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
compensateExposure cam rad = cam.exposure *> rad

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
parseLines (l:ls) = p:parseLines ls
  where
    p = case parse sline "rt camera file parse error" l of
        Left  e  -> error (show e ++ "\nLINE: " ++ l)
        Right p' -> p'

generateRay :: Camera -> (Double, Double) -> IO Ray
generateRay cam (y, x) = do
  blur <- lensOffset cam.blurFlag cam.lens
  --putStrLn ("OR:" ++ show origin ++ ", BL:" ++ show blur)
  --putStrLn ("BL:" ++ show blur)
  (r3, r4) <- if cam.antialias
    then do
      r3' <- MT.randomIO :: IO Double
      r4' <- MT.randomIO :: IO Double
      return (r3' - 0.5, r4' - 0.5)
    else
      return (0.0, 0.0)
  let
    eyepos = cam.eyePos + blur
    (esx, esy) = cam.screenStep
    eyedir = cam.screenOrigin + (x + r3) *> esx + (y + r4) *> esy - blur
  return (initRay eyepos (fromJust $ normalize eyedir))

lensOffset :: Bool -> (Direction3, Direction3) -> IO Direction3
lensOffset blurflag (lens_x, lens_y) =
  if blurflag
    then do
      r1 <- MT.randomIO :: IO Double
      r2 <- MT.randomIO :: IO Double
      -- 方法1) 正方形にランダムな点を取り、半径1の円内ならOK、外ならやり直し
      --let
      --  ofx = 2.0 * (r1 - 0.5)
      --  ofy = 2.0 * (r2 - 0.5)
      --if ofx * ofx + ofy * ofy <= 1.0
      --  then return (ofx *> lens_x + ofy *> lens_y)
      --  else lensOffset blurflag (lens_x, lens_y)

      -- 方法2) 二次元極座標を使う
      -- r1=半径、r2=角度
      -- https://techblog.kayac.com/how-to-distribute-points-randomly-using-high-school-math
      let
        r = sqrt r1
        theta = r2 * pi2
        ofx = r * cos theta
        ofy = r * sin theta
      return (ofx *> lens_x + ofy *> lens_y)
    else return o3

pnmHeader0 :: (Int, Int) -> Double -> [String]
pnmHeader0 (xr, yr) maxrad =
  ["P3"
  ,"## max radiance = " ++ show maxrad
  ,show xr ++ " " ++ show yr
  ,"255"
  ]

{--
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
        r'  = min d' 1.0 ** gamma
--}
