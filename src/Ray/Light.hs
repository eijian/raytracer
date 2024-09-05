{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

--
-- Light
--

module Ray.Light (
  RadEstimation (..)
, LightSpec (..)
, decayEmittance
, generatePhoton
--, getDirection
, getRadiance
, initLightSpec
, lemittance
--, validPoint
) where

--import System.Random
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
--import Data.Maybe
--import Debug.Trace
import GHC.Generics
import NumericPrelude
import System.Random.Mersenne as MT

import Ray.Algebra
import Ray.Geometry
import Ray.Optics
import Ray.Physics

type Radiosity = Double

{-
Light: 光源型、物体形状と分離して光源の仕様のみとした。

参考光源情報）
  Panasonic (https://panasonic.jp/lamp/)
  電球型(LDA13(L/N/D)-G/Z100E/S/W, LDA7(L/WW/N/D)-D-G/S/Z6)
    電球色  温白色  昼白色  昼光色
    2700K   3500K   5000K   6500K 

    100W    60W     40W
    1520lm   810lm   485lm
  
  丸型(FCL30EL28MF2)
    電球色   ナチュラル色  クール色
    3000K    5200K         6200K

    30W     32W     40W
    2100lm  2480lm  3230lm


  Panasonic 丸型LED 95mm
  LDG11L-G/95/W  1370lm -> 46,320 lx (lm/m^2)

  Panasonic 円環30形 (225-167,29mm)
  FCL30EDW28MF3  2100lm -> 74,868 lx (lm/m^2)

-}

{-|
  直接光の放射輝度推定方法 
-}
data RadEstimation =
    Formula   -- 古典レイトレーシング=計算で求める
  | PhotonMap -- フォトンマップから輝度推定する
  deriving (Eq, Show, Generic)

instance NFData RadEstimation where
  rnf :: RadEstimation -> ()
  rnf = genericRnf

data LightSpec = LightSpec
  { lcolor      :: !Color         -- RGBの比率、r+g+b = 1.0
  , radiosity   :: !Radiosity     -- 放射発散度 [W/m^2]
  , directivity :: !Double        -- 指向性 (0.0:指向性なし - 1.0:平行光源)
  , radest      :: !RadEstimation -- 直接光の輝度値計算方法
  , dirflag     :: !InOut         -- 発光方向（通常光源=Out, ドーム光源等=In)
  -- calcuration when initializing
  , cospower    :: !Double
  , power       :: !Double
  , emittance0  :: !Radiance      -- 放射輝度（面法線方向）
  }
  deriving (Eq, Show, Generic)

instance NFData LightSpec where
  rnf :: LightSpec -> ()
  rnf = genericRnf

{-}
data Light = Light
  { spec       :: !LightSpec     -- RGBの比率、r+g+b = 1.0
  , lshape     :: !Shape         -- 光源形状
  , nsample    :: Int
  }
  deriving (Eq, Show, Generic)

instance NFData Light where
  rnf = genericRnf
-}

initLightSpec :: Color -> Radiosity -> Double -> RadEstimation -> InOut
  -> LightSpec
initLightSpec col lux direct radest dirf =
  LightSpec col radiosity direct radest dirf cpow pow em
  where
    radiosity = lux / 683.0   -- flux [W] = lumen / 683.0, lux = lumen / S [lumen/m^2]
    (cpow, pow)  = densityPower (direct ** 3)
    em0 = srHalf * radiosity
    em  = em0 *> col <**> radiance1

{-
initLight :: LightSpec -> Shape -> Light
initLight spec@(LightSpec _ _ direct _ _ _ _ _) shape = Light spec shape nsam
  where
    mag  = truncate (1.5 ** (direct * 10))
    n    = nSurface shape
    nsam = if mag < 1 then n else mag * n
    --nsam = if ns < 1 then 1 else ns
-}

lemittance :: LightSpec -> SurfacePoint -> Direction3 -> Radiance
lemittance (LightSpec _ _ _ _ _ _ pow em) (_, nvec) vvec = cos' *> em
  where
    cos = nvec <.> vvec
    cos' = (-cos) ** (0.5 / pow)

generatePhoton :: LightSpec -> SurfacePoint -> IO (Photon, RadEstimation)
generatePhoton (LightSpec col _ _ radest dirf _ pow _) (pos, nvec) = do
  wl <- MT.randomIO :: IO Double
  let
    nvec2 = if dirf == Out
      then nvec
      else negate nvec
  nvec' <- blurredVector nvec2 pow
  return ((decideWavelength col wl, initRay pos nvec'), radest)

{-
generatePhoton :: Light -> IO (Photon, RadEstimation)
generatePhoton (Light (LightSpec col _ _ radest dirf _ pow _) shp _) = do
  wl <- MT.randomIO :: IO Double
  (pos, nvec) <- randomPoint shp
  let
    w = decideWavelength col wl
    nvec2 = if dirf == Out
      then nvec
      else negate nvec
  nvec' <- blurredVector nvec2 pow
  return ((w, initRay pos nvec'), radest)
-}

{-
getDirection :: Light -> Position3 -> Position3 -> Maybe Direction3
getDirection (Light _ _ _ shape _ _ _ _ _ _) lpos pos
  | nvec == Nothing = Nothing
  | cos > 0.0       = Nothing
  | otherwise       = Just lvec
  where
    nvec = getNormal lpos shape
    lvec = lpos - pos
    cos = (fromJust nvec) <.> lvec
-}

{-
光源までの距離は二乗された状態で入力される。1/4πd となっているがdは実際はd^2。
-}

getRadiance :: LightSpec -> Direction3 -> Direction3 -> Radiance
getRadiance (LightSpec (Color r g b) radiosity _ _ _ cpow _ _) lnvec lvec
  = Radiance (r * decay) (g * decay) (b * decay)
  where
    cos = lnvec <.> lvec
    decay = srHalf * radiosity * (cos ** cpow) * (cpow + 1.0)

decayEmittance :: LightSpec -> Double -> Radiance
decayEmittance (LightSpec _ _ _ _ _ cpow _ em) cos = decay *> em
  where
    decay =  (cpow + 1.0) * (cos ** cpow)

{-

-}

{-
validPoint :: Light -> SurfacePoint -> IO SurfacePoint
validPoint lgt (pos, nvec) = do
  sfpt@(lpos, lnvec) <- randomPoint (lshape lgt)
  return sfpt
--  if lnvec <.> nvec < 0.0  -- 面が光源を向いていない
--    then return Nothing
--    else return (Just sfpt)
-}

{-
    else do
      let
        ldir = lpos - pos
      p <- if ldir <.> nvec < 0.0
        then validPoint lgt pos nvec
        else return (lpos, lnvec)
      return p
-}
