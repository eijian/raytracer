{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

--
-- Optics
--

{-
  光束の単位「 lm 」について

  大きな考え方としては、「放射束」を人間の眼で見たときの「明るさ」が「光束」
  なのですが、この関係をもう少し厳密に説明します。正確には、放射量（ワット
  単位）から測光量（ルーメン単位）への変換には、最大視感効果度 Km と呼ばれる
  変換定数を掛ける必要があります。
  
     Km ＝ 683 [ lm / W ]

  cf: https://www.ccs-inc.co.jp/guide/column/light_color/vol04.html

--
  ８畳間では3300-4300ルーメンぐらい。flux=5.0だと上の換算値から、

    5.0[W] x 683[lm/W] = 3415[lm]
  
  となり、妥当な数字。

--
  作例の天井ライトは1.34[m]四方、5.0[W]である。光は下面のみから放射されるため
  半球に広がる(1/2π[1/sr])となる。よってこのライトの放射輝度は、

    5.0 / 2π / (1.34^2) = 0.4421 [W/sr/m^2]

    米 これを光の三原色で均等割するのか、それぞれがこの値か？

-}

module Ray.Optics (
  Photon
, PhotonCache
, PhotonFilter (..)
, Radiance (Radiance)
, convertToInfo
, diffuseReflection
, elemR
, elemG
, elemB
, fresnelReflectance
, fresnelReflectanceColor
, initPhoton
, photonDir
, photonDummy
, photonPos
, photonToRadiance
, rabs'
, radiance0
, radiance1
, specularReflection
, specularRefraction
, squaredDistance
, (<**>)
) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           Debug.Trace
import           Data.Maybe
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
--import qualified Data.Vector as V
--import           Data.Vector.Generic.Base
--import           Data.Vector.Generic.Mutable  hiding (read)
--import qualified Data.Vector.Unboxed as VU
import           GHC.Generics 
import           NumericPrelude
import           Test.QuickCheck

import           Ray.Algebra
import           Ray.Geometry
import           Ray.Physics

--
-- Photon Filter
--

data PhotonFilter =
    Nonfilter
  | Conefilter
  | Gaussfilter
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData PhotonFilter where
  rnf = genericRnf                  

--
-- Radiance

data Radiance = Radiance !Double !Double !Double
  deriving (Read, Show, Generic)

instance NFData Radiance where
  rnf = genericRnf
                
instance Eq Radiance where
  (Radiance r1 g1 b1) == (Radiance r2 g2 b2)
    = r1 == r2 && g1 == g2 && b1 == b2

instance Arbitrary Radiance where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ Radiance r g b

{- |

>>> let r1 = Radiance 0.01 0.02 0.005
>>> let r2 = Radiance 0.008 0.015 0.009
>>> norm (r1 - r2)
1.1e-2
-}
instance Additive.C Radiance where
  zero = Radiance 0 0 0
  (Radiance r1 g1 b1) + (Radiance r2 g2 b2)
    = Radiance (r1 + r2) (g1 + g2) (b1 + b2)
  (Radiance r1 g1 b1) - (Radiance r2 g2 b2)
    = Radiance (r1 - r2) (g1 - g2) (b1 - b2)

instance Module.C Double Radiance where
  s *> (Radiance r g b) = Radiance (s * r) (s * g) (s * b)

instance BasicMatrix Radiance where
  norm (Radiance r g b) = rabs r + rabs g + rabs b
  a .=. b = norm (a - b) < nearly0

rabs :: Double -> Double
rabs d = if d < 0.0 then (-d) else d

{- |
>>> rabs' (Radiance 0.001 (-0.02) (-0.00005))
Radiance 1.0e-3 2.0e-2 5.0e-5
-}

rabs' :: Radiance -> Radiance
rabs' (Radiance r g b) = Radiance (rabs r) (rabs g) (rabs b)

(<**>) :: Color -> Radiance -> Radiance
(Color cr cg cb) <**> (Radiance r g b)
  = Radiance (cr * r) (cg * g) (cb * b)
infix 8 <**>

elemRad :: Wavelength -> Radiance -> Double
elemRad Red   (Radiance r _ _) = r
elemRad Green (Radiance _ g _) = g
elemRad Blue  (Radiance _ _ b) = b

elemR :: Radiance -> Double
elemR = elemRad Red
elemG :: Radiance -> Double
elemG = elemRad Green
elemB :: Radiance -> Double
elemB = elemRad Blue

radiance0 :: Radiance
radiance0 = Radiance 0 0 0
radiance1 :: Radiance
radiance1 = Radiance 1 1 1

-- | Radiance
-- >>> let a = Radiance 0.3 0.8 0.3
-- >>> let b = Radiance 1.2 0.5 2.5
-- >>> a + b
-- Radiance 1.5 1.3 2.8


--
-- Photon

type Photon = (Wavelength, Ray)

initPhoton :: Wavelength -> Ray -> Photon
initPhoton l r = (l, r)

type PhotonCache = Photon

photonDummy :: Position3 -> Photon
photonDummy p = (Red, (p, ex3))

photonDir :: Photon -> Direction3
photonDir (_, (_, d)) = d

photonPos :: Photon -> Position3
photonPos (_, (p, _)) = p

convertToInfo :: Photon -> Photon
convertToInfo (wl, (rp, rd)) = (wl, (rp, (negate rd)))

squaredDistance :: Photon -> Photon -> Double
squaredDistance (_, (v1, _)) (_, (v2, _)) = d <.> d
  where
    d = v1 - v2

photonToRadiance :: Direction3 -> Double -> Photon -> Radiance
photonToRadiance n pw (wl, (_, d)) =
  case wl of
    Red   -> Radiance pw' 0 0
    Green -> Radiance 0 pw' 0
    Blue  -> Radiance 0 0 pw'
  where
    cos0 = n <.> d
    pw'  = if cos0 > 0.0 then pw else 0.0

--
-- REFLECTION AND REFRACTION
--

diffuseReflection :: Direction3 -> IO Direction3
diffuseReflection n = do
  dir <- generateRandomDir4
  let c = n <.> dir
  return $ if c > 0.0 then dir else negate dir

{-
specular reflection
  IN : nvec  = Normal vector (from surface)
       vvec  = eye direction (to surface)
  OUT: rvec  = reflection vector (from surface)
       cos1  = (rvec, nvec)
  条件: cos = (N, V) は正にならないといけない。万が一そうでなければNを返す。
-}
specularReflection :: Direction3 -> Direction3 -> (Direction3, Double)
specularReflection nvec vvec
  | cos < 0.0       = (nvec, -cos)
  | rvec == Nothing = (nvec, 0.0)
  | otherwise       = (fromJust rvec,  cos)
  where
    cos = -vvec <.> nvec
    rvec = normalize (vvec + (2.0 * cos) *> nvec)

{-

  http://kanamori.cs.tsukuba.ac.jp/jikken/inner/reflection_refraction.pdf

  T = 1/η {L + (c - g) N}

    c = cos θ1 = -(L.N)
    g = √(η^2 + c^2 - 1)
    η = η2 / η1 

    N      <- nvec
    L      <- vvec
    η      <- eta
    cos θ1 <- cos1 (=-(L.N))

  OUT: tvec = T
       cos2 = (T.-N)

-}

specularRefraction :: Direction3 -> Direction3 -> Double -> Double
                   -> (Maybe Direction3, Double)
specularRefraction nvec vvec eta cos
  | cos < 0.0       = (Nothing, 0.0) 
  | g0 <  0.0       = (Nothing, 0.0)  -- 全反射
  | tvec == Nothing = (Nothing, 0.0)
  | otherwise       = (tvec, g / eta)
  where
    g0 = eta * eta + cos * cos - 1.0
    g  = sqrt g0
    tvec = normalize ((1.0 / eta) *> (vvec + (cos - g) *> nvec))

--
-- UTILITY
--

{-
fresnelReflectance: フレネル反射率計算
  schlickの近似式を用いてF(0°)から計算する
  cf: https://hanecci.hatenadiary.org/entry/20130525/p3
-}

fresnelReflectanceColor :: Color -> Double -> Color
fresnelReflectanceColor (Color r g b) cos =
  Color (r + (1.0 - r) * cos') (g + (1.0 - g) * cos') (b + (1.0 - b) * cos')
  where
    cos' = (1.0 - cos) ** 5.0
--fresnelReflectanceColor col@(Color r g b) cos = col + cos' *> (negate col)
--  where
--    cos' = (1.0 - cos) ** 5.0

fresnelReflectance :: Double -> Double -> Double
fresnelReflectance f0 cos = f0 + (1.0 - f0) * (1.0 - cos) ** 5.0




