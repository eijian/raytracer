{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Physics
--

module Ray.Physics (
  Color (Color)
, Wavelength (Red, Green, Blue)
, black
, distributedNormal
, expColor
, initColor
, normalizeColor
, decideWavelength
, selectWavelength
, negateColor
, scaleColor
, addColor
, mulColor
, relativeIorAverage
, relativeIorWavelength
, reflectionGlossy
, russianRoulette
, white
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Maybe
import           GHC.Generics
import           NumericPrelude
import           System.Random.Mersenne as MT

import           Ray.Algebra

--
-- Wavelength

{-
波長
  Red  : 700.0 nm
  Green: 546.1 nm
  Blue : 435.5 nm
-}
data Wavelength = Red | Green | Blue deriving (Show, Read, Enum, Eq, Generic)

instance NFData Wavelength where
  rnf = genericRnf

--
-- Color

data Color = Color !Double !Double !Double deriving (Generic)

instance NFData Color where
  rnf = genericRnf

black :: Color
black = Color 0.0 0.0 0.0
white :: Color
white = Color 1.0 1.0 1.0

instance Show Color where
  show (Color r g b) = "[" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "]"

instance Eq Color where
  (Color ar ag ab) == (Color br bg bb) = (ar == br) && (ag == bg) && (ab == bb)

initColor :: Double -> Double -> Double -> Color
initColor r g b
  | mag == 0  = Color (1/3) (1/3) (1/3)
  | otherwise = Color (r'/mag) (g'/mag) (b'/mag)
  where
    r' = clipColor r
    g' = clipColor g
    b' = clipColor b
    mag = r' + g' + b'

normalizeColor :: Color -> Color
normalizeColor (Color r g b)
  | mag == 0  = Color (1/3) (1/3) (1/3)
  | otherwise = Color (r'/mag) (g'/mag) (b'/mag)
  where
    r' = clipColor r
    g' = clipColor g
    b' = clipColor b
    mag = r' + g' + b'

clipColor :: Double -> Double
clipColor a
  | a < 0     = 0
  | otherwise = a

decideWavelength :: Color -> Double -> Wavelength
decideWavelength (Color r g _) p
  | p < r     = Red
  | p < r + g = Green
  | otherwise = Blue

selectWavelength :: Wavelength -> Color -> Double
selectWavelength Red   (Color r _ _) = r
selectWavelength Green (Color _ g _) = g
selectWavelength Blue  (Color _ _ b) = b

negateColor :: Color -> Color
negateColor (Color r g b) = Color (1.0 - r) (1.0 - g) (1.0 - b)

scaleColor :: Double -> Color -> Color
scaleColor s (Color r g b) = Color (s * r) (s * g) (s * b)

addColor :: Color -> Color -> Color
addColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)

mulColor :: Color -> Color -> Color
mulColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)

expColor :: Color -> Double -> Color
expColor (Color r g b) e = Color (r ** e) (g ** e) (b ** e)

-- Physics Lows -----------------

-- relative of IoR
--   n1: IoR of src
--   n2: IoR of dst
--   eta = n2 / n1

relativeIor :: Double -> Double -> Double
relativeIor ior1 ior2
  | ior1 == 0.0 = 1.0
  | otherwise   = ior2 / ior1

relativeIorColor :: Color -> Color -> Color
relativeIorColor (Color r1 g1 b1) (Color r2 g2 b2) =
  Color (relativeIor r1 r2) (relativeIor g1 g2) (relativeIor b1 b2)

relativeIorWavelength :: Color -> Color -> Wavelength -> Double
relativeIorWavelength (Color r1 g1 b1) (Color r2 g2 b2) wl =
  case wl of
    Red   -> relativeIor r1 r2
    Green -> relativeIor g1 g2
    Blue  -> relativeIor b1 b2

relativeIorAverage :: Color -> Color -> Double
relativeIorAverage (Color r1 g1 b1) (Color r2 g2 b2) =
  relativeIor a1 a2
  where
    a1 = (r1 + g1 + b1) / 3.0
    a2 = (r2 + g2 + b2) / 3.0

{- |
specular reflection
  IN : nvec  = Normal vector (from surface)
       vvec  = eye direction (to surface)
  OUT: rvec  = reflection vector (from surface)
       cos1  = (rvec, nvec)
  条件: cos = (N, V) は正にならないといけない。万が一そうでなければNを返す。
-}

specularReflection :: Direction3 -> Direction3 -> (Direction3, Double)
specularReflection nvec vvec =
  if cos >= 0.0
    then (fromJust $ normalize (vvec + (2.0 * cos) *> nvec), cos)
    else (nvec, -cos)
  where
    cos = -vvec <.> nvec  -- -(E,N)

{-
glossyな表面：法線ベクトルがブレていると捉える
  ブレ幅は表面の粗さ(roughness)に依る。
    完全に粗い  ：roughness=1.0
    全く粗くない：roughness=0.0
  
  面の法線をN、glossy面でブレた法線をN'とすると
  <N',N'>(NとN'のなす角をθとした時のcosθ)は、乱数ξ[0,1]を用いて
    cosθ = ξ^(1/n+1)
  とする。ここでnは
    n = 10^(5*(1-√roughness))
  と決める。（roughness 0 〜 1の時の増分に結果がスライドするように）
-}

distributedNormal :: Direction3 -> Double -> IO Direction3
distributedNormal nvec pow = do
  xi1 <- MT.randomIO :: IO Double    -- horizontal
  xi2 <- MT.randomIO :: IO Double    -- virtical
  let
    phi = 2.0 * pi * xi2
    uvec0 = normalize $ nvec <*> (Vector3 0.00424 1.0 0.00764)
    uvec = case uvec0 of
      Just v  -> v
      Nothing -> fromJust $ normalize $ nvec <*> (Vector3 1.0 0.00424 0.00764)
    vvec = uvec <*> nvec
    xi1' = xi1 ** pow
    rt = sqrt (1.0 - xi1' * xi1')

    x = cos(phi) * rt
    y = xi1'
    z = sin(phi) * rt

    nvec' = x *> uvec + y *> nvec + z *> vvec
--    nvec' = if nvec <.> wi < 0.0
--      then negate wi
--      else wi
  case normalize nvec' of
    Just v  -> return v
    Nothing -> return ex3    

{-
glossyな表面の反射ベクトルの求め方
  http://www.raytracegroundup.com/downloads/Chapter25.pdf
  https://cg.informatik.uni-freiburg.de/course_notes/graphics2_08_renderingEquation.pdf
  https://graphics.cg.uni-saarland.de/courses/ris-2018/slides/09_BRDF_LightSampling.pdf
  x = cos(2 pi xi2) sqrt(1 - xi1^(2/n+1))
  y = xi1^(1/n+1)
  z = sin(2 pi xi2) sqrt(1 - xi1^(2/n+1))
    where xi1 = cos (w.r) ^ 10 ^ (5 x (1 - sqrt(roughness)))
-}

reflectionGlossy :: Direction3 -> Direction3 -> Double -> IO Direction3
reflectionGlossy nvec rvec pw = do
  r1 <- MT.randomIO :: IO Double
  r2 <- MT.randomIO :: IO Double
  let
    uvec0 = normalize $ (Vector3 0.00424 1.0 0.00764) <*> rvec
    uvec = case uvec0 of
      Just v  -> v
      Nothing -> fromJust $ normalize $ (Vector3 1.0 0.00424 0.00764) <*> rvec
    vvec = uvec <*> rvec

    -- 試行として入射角に応じて反射ベクトルの分散度を変化させる(cosを掛ける)
    c0 = nvec <.> rvec
    xi1 = r1 ** (pw * c0)
    xi2 = 2.0 * pi * r2
    rt = sqrt (1.0 - xi1 * xi1)
    x = cos(xi2) * rt
    y = xi1
    z = sin(xi2) * rt

    wi = x *> uvec + y *> rvec + z *> vvec
    wi2 = if nvec <.> wi < 0.0
      then (-x) *> uvec + y *> rvec + (-z) *> vvec
      else wi
  case normalize wi2 of
    Just v  -> return v
    Nothing -> return ex3

{-
pecular_rafraction
  IN : nvec  = Normal vector (from surface)
       vvec  = eye direction (to surface)
       eta   = relative of IoR (eta = n2 / n1)
  OUT: tvec  = refraction vector (from surface)
       cos2  = (tvec, -nvec)
-}

specularRefraction :: Direction3 -> Direction3 -> Double -> (Maybe Direction3, Double)
specularRefraction nvec vvec eta
  | cos1 < 0.0 = (Nothing, 0.0)
  | g0 < 0.0   = (Nothing, 0.0)
  | otherwise  = (normalize ((1.0 / eta) *> (vvec + (cos1 - g) *> nvec)), g / eta)
  where
    cos1 = -vvec <.> nvec -- -(E,N)
    sq_eta = eta * eta
    sq_cos = cos1 * cos1
    g0 = sq_eta + sq_cos - 1.0
    g = sqrt g0

{- |
Snell's low
  IN : r_ior = relative of IoR (n = n2/n1)
       nvec  = Normal vector (from surface)
       vvec  = eye direction (to surface)
  OUT: R  reflection dir
       T  refraction dir
       cos1  reflection cosine
       cos2  refraction cosine
-}

snell :: Double -> Direction3 -> Direction3
  -> (Direction3, Direction3, Double, Double)
snell r_ior nvec vvec
  | g <= 0.0  = (r, o3, c1, 0.0)   -- 全反射
  | otherwise = (r, t, c1, c2)
  where
    c1 = vvec <.> nvec
    r = fromJust $ normalize (vvec - (2.0 * c1) *> nvec)
    n = r_ior * r_ior
    g = 1.0 / n + c1 * c1 - 1.0

    a = -c1 - sqrt g
    t = fromJust $ normalize (r_ior *> (vvec + a *> nvec))
    c2 = sqrt (1.0 - n * (1.0 - c1 * c1))


{- |
russianRoulette

>>> russianRoulette [0.5]
0

-}

russianRoulette :: [Double] -> IO Int
russianRoulette cs = do
  rnd <- MT.randomIO :: IO Double
  return $ rr cs 0.0 rnd (length cs)

rr :: [Double] -> Double -> Double -> Int -> Int
rr [] _ _ len = len
rr (c:cs) c0 rnd len
  | rnd < c'  = len
  | otherwise = rr cs c' rnd (len-1)
  where
    c' = c0 + c

{-
reflectionIndex :: Color -> Double -> Color
reflectionIndex (Color r g b) c =
  Color (r + (1-r) * c') (g + (1-g) * c') (b + (1-b) * c')
  where
    c' = (1.0 - c) ** 5.0
-}