{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Light
--

module Ray.Light (
  RadEstimation (..)
, Light (..)
, generatePhoton
, getDirection
, getRadiance
, initLight
, lemittance
, validPoint
) where

--import System.Random
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.Maybe
import           Debug.Trace
import GHC.Generics
import NumericPrelude
import System.Random.Mersenne as MT

import Ray.Algebra
import Ray.Geometry
import Ray.Optics
import Ray.Physics

type Flux = Double

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

-}

{-|
  直接光の放射輝度推定方法 
-}
data RadEstimation =
    Formula   -- 古典レイトレーシング=計算で求める
  | PhotonMap -- フォトンマップから輝度推定する
  deriving (Eq, Show, Generic)

instance NFData RadEstimation where
  rnf = genericRnf

data Light = Light
  { lcolor      :: !Color         -- RGBの比率、r+g+b = 1.0
  , flux        :: !Flux          -- 光束 [W/?]
  , directivity :: !Double        -- 指向性 (0.0:指向性なし - 1.0:平行光源)
  , lshape      :: !Shape         -- 光源形状
  , radest      :: !RadEstimation -- 直接光の輝度値計算方法
  , dirflag     :: !InOut         -- 発光方向（通常光源=Out, ドーム光源等=In)
  -- calcuration when initializing
  , cospower    :: !Double
  , power       :: !Double
  , emittance0  :: !Radiance
  , nsample     :: Int
  }
  deriving (Eq, Show, Generic)

instance NFData Light where
  rnf = genericRnf

initLight :: Color -> Flux -> Double -> Shape -> RadEstimation -> InOut -> Light
initLight col lumen direct shape radest dirf =
  Light col flux direct shape radest dirf cpow pow em nsam
  where
    flux = lumen / 683.0
    (cpow, pow)  = densityPower (direct ** 3)
    e0   = sr_half * flux / surfaceArea shape
    em   = (3.0 * e0) *> col <**> radiance1
    ns   = truncate (1.5 ** (direct * 10))
    nsam = if ns < 1 then nSurface shape else ns * nSurface shape
    --nsam = if ns < 1 then 1 else ns

lemittance :: Light -> SurfacePoint -> Direction3 -> Radiance
lemittance (Light _ _ _ _ _ _ _ pow em _) (_, nvec) vvec = cos' *> em
  where
    cos = nvec <.> vvec
    cos' = (-cos) ** (0.5 / pow)

generatePhoton :: Light -> IO (Photon, RadEstimation)
generatePhoton (Light c _ _ s radest dirf _ pow _ _) = do
  wl <- MT.randomIO :: IO Double
  (pos, nvec) <- randomPoint s
  let
    w = decideWavelength c wl
    nvec2 = if dirf == Out
      then nvec
      else negate nvec
  nvec' <- blurredVector nvec2 pow
  return ((w, initRay pos nvec'), radest)

getDirection :: Light -> Position3 -> Position3 -> Maybe Direction3
getDirection (Light _ _ _ shape _ _ _ _ _ _) lpos pos
  | nvec == Nothing = Nothing
  | cos > 0.0       = Nothing
  | otherwise       = Just lvec
  where
    nvec = getNormal lpos shape
    lvec = lpos - pos
    cos = (fromJust nvec) <.> lvec

{-
光源までの距離は二乗された状態で入力される。1/4πd となっているがdは実際はd^2。
-}

getRadiance :: Light -> Direction3 -> Direction3 -> Radiance
getRadiance lgt@(Light (Color r g b) f _ _ _ _ cpow _ _ _) lnvec lvec
  = Radiance (r * decay) (g * decay) (b * decay)
  where
    cos = lnvec <.> lvec
    decay = f * (cos ** cpow) * (cpow + 1.0) / pi2

{-

-}

validPoint :: Light -> SurfacePoint -> IO SurfacePoint
validPoint lgt (pos, nvec) = do
  sfpt@(lpos, lnvec) <- randomPoint (lshape lgt)
  return sfpt
--  if lnvec <.> nvec < 0.0  -- 面が光源を向いていない
--    then return Nothing
--    else return (Just sfpt)

{-
    else do
      let
        ldir = lpos - pos
      p <- if ldir <.> nvec < 0.0
        then validPoint lgt pos nvec
        else return (lpos, lnvec)
      return p
-}
