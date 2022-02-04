{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Light
--

module Ray.Light (
  Light (..)
, generatePhoton
, getDirection
, getRadiance
, initLight
, lemittance
) where

--import System.Random
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.Maybe
--import           Debug.Trace
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

data Light = Light
  { lcolor      :: !Color
  , flux        :: !Flux
  , directivity :: !Double
  , lshape      :: !Shape
  , dirflag     :: !Bool
  -- calcuration when initializing
  , power       :: !Double
  , emittance0  :: !Radiance
  }
  deriving (Eq, Show, Generic)

instance NFData Light where
  rnf = genericRnf

initLight :: Color -> Flux -> Double -> Shape -> Bool -> Light
initLight col lumen direct shape dirf = Light col flux direct shape dirf pow em
  where
    flux = lumen / 683.0
    pow  = densityPower (direct ** 3)
    e0   = sr_half * flux / surfaceArea shape
    em   = (3.0 * e0) *> col <**> radiance1

lemittance :: Light -> Position3 -> Direction3 -> Direction3 -> Radiance
lemittance (Light _ _ _ _ _ pow em) pos nvec vvec = cos' *> em
  where
    cos = nvec <.> vvec
    cos' = (-cos) ** (0.5 / pow)

generatePhoton :: Light -> IO Photon
generatePhoton (Light c _ _ s flag pow _) = do
  wl <- MT.randomIO :: IO Double
  (pos, nvec) <- randomPoint s
  let
    w = decideWavelength c wl
    nvec2 = if flag == True
      then nvec
      else negate nvec
  
  nvec' <- blurredVector nvec2 pow
  return (w, initRay pos nvec')

getDirection :: Light -> Position3 -> Position3 -> Maybe Direction3
getDirection (Light _ _ _ shape _ _ _) lpos pos
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

getRadiance :: Light -> Position3 -> Position3
  -> (Double, Maybe Direction3, Radiance)
getRadiance lgt@(Light (Color r g b) f _ shape _ pow _) lpos pos
  | ldir0 == Nothing = (0.0, Nothing, radiance0)
  | lvec == Nothing  = (0.0, Nothing, radiance0)
  | cos < 0.0        = (0.0, Nothing, radiance0)
  | otherwise        = (dist, lvec, rad)
  where
    nvec = getNormal lpos shape
    ldir0 = getDirection lgt lpos pos
    ldir = fromJust ldir0
    lvec = normalize $ ldir
    cos = (fromJust nvec) <.> (negate $ fromJust lvec)
    dist = square ldir
    decay = (1.0 / (pi4 * dist)) ** (2.0 * pow)
    mag = decay * 3.0 * f * (cos ** (0.5 / pow))
    rad = Radiance (mag * r) (mag * g) (mag * b)

