{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Light
--

module Ray.Light (
  Light (..)
, flux
, directivity
, initLight
, lemittance
, generatePhoton
, getDirection
, getRadiance
, lshape
) where

--import System.Random
import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           System.Random.Mersenne as MT
import           Data.Maybe
import           Debug.Trace
import           GHC.Generics
import           NumericPrelude


import Ray.Algebra
import Ray.Geometry
import Ray.Physics
import Ray.Optics

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
  -- calcuration when initializing
  , power       :: !Double
  , emittance0  :: !Radiance
  }
  deriving (Eq, Show, Generic)

instance NFData Light where
  rnf = genericRnf

{-  
  PointLight
  { lcolor :: Color
  , lflux  :: Flux
  , pos    :: Position3
  }
  |
  ParallelogramLight
  { lcolor :: Color
  , lflux  :: Flux
  , pos    :: Position3
  , nvec   :: Direction3
  , dir1   :: Direction3
  , dir2   :: Direction3
  }
  |
  SunLight
  { lcolor :: Color
  , lflux  :: Flux
  , pos    :: Position3
  , nvec   :: Direction3
  , dir1   :: Direction3
  , dir2   :: Direction3
  , ldir   :: Direction3
  }
-}

--instance Show Light where
--  show (Light c f d s p e) = "[" ++ show c ++ "," ++ show f ++ "," ++ show d ++ "," ++ show s ++ "," ++ show p ++ "," ++ show e ++ "]"
  {-
  show (PointLight c f p) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "]"
  show (ParallelogramLight c f p n d1 d2) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "," ++ show n ++ "," ++ show d1 ++ "," ++ show d2 ++ "]"
  show (SunLight c f p n d1 d2 d0) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "," ++ show n ++ "," ++ show d1 ++ "," ++ show d2 ++ "," ++ show d0 ++ "]"
  -}

initLight :: Color -> Flux -> Double -> Shape -> Light
initLight col lumen direct shape = Light col flux direct shape pow em
  where
    flux = lumen / 683.0
    pow  = densityPower (direct ** 3)
    e0   = sr_half * flux / surfaceArea shape
    em   = (3.0 * e0) *> col <**> radiance1

lemittance :: Light -> Position3 -> Direction3 -> Radiance
lemittance (Light _ _ _ shape pow em) pos vvec = cos' *> em
  where
    nvec = getNormal pos shape
    cos = (fromJust nvec) <.> vvec
    cos' = (-cos) ** (0.5 / pow)

{-
flux :: Light -> Flux
flux (PointLight _ f _) = f
flux (ParallelogramLight _ f _ _ _ _) = f
flux (SunLight _ f _ _ _ _ _) = f
-}

generatePhoton :: Light -> IO Photon
generatePhoton (Light c _ d s pow _) = do
  wl <- MT.randomIO :: IO Double
  pos <- randomPoint s
  let
    w = decideWavelength c wl
    nvec = getNormal pos s
  --putStrLn ("lpos=" ++ show pos ++ "/shape=" ++ show s)
  nvec' <- blurredVector (fromJust nvec) pow
  return (w, initRay pos nvec')

{-
generatePhoton (PointLight c _ p) = do
  --wl <- randomRIO (0, 1.0)
  wl <- MT.randomIO :: IO Double
  d  <- generateRandomDir4
  let r = initRay p d
      w = decideWavelength c wl
  return (w, r)
generatePhoton (ParallelogramLight c _ p n d1 d2) = do
  wl <- MT.randomIO :: IO Double
  t1 <- MT.randomIO :: IO Double
  t2 <- MT.randomIO :: IO Double
  d  <- diffuseReflection n
  let r = initRay (p + t1 *> d1 + t2 *> d2) d
      w = decideWavelength c wl
  return (w, r)
generatePhoton (SunLight c _ p n d1 d2 d0) = do
  wl <- MT.randomIO :: IO Double
  t1 <- MT.randomIO :: IO Double
  t2 <- MT.randomIO :: IO Double
  let r = initRay (p + t1 *> d1 + t2 *> d2) d0
      w = decideWavelength c wl
  return (w, r)
-}

getDirection :: Light -> Position3 -> Position3 -> Maybe Direction3
getDirection (Light _ _ _ shape _ _) lpos pos
  | nvec == Nothing = Nothing
  | cos > 0.0       = Nothing
  | otherwise       = Just lvec
  where
    nvec = getNormal lpos shape
    lvec = lpos - pos
    cos = (fromJust nvec) <.> lvec
{-
getDirection (PointLight _ _ lp) p _ = [lp - p]
getDirection (ParallelogramLight _ _ lp n d1 d2) p (x, y)
  | n <.> d >= 0.0 = []
  | otherwise = [d]
  --filter (\d -> n <.> d < 0.0) $ map (\(tx, ty) -> genPos tx ty - p) tss
  where
    d = genPos x y - p
    genPos :: Double -> Double -> Position3
    genPos tx ty = lp + tx *> d1 + ty *> d2
getDirection (SunLight _ _ lp n d1 d2 dt) p _
  | cos0 > 0.0     = []
  | res == Nothing = []
  | otherwise      = [t *> dt']
  where
    d = lp - p
    cos0 = n <.> d
    dt' = negate dt
    res = methodMoller 2.0 lp d1 d2 p dt'
    (_, _, t) = fromJust res
-}

{-
光源までの距離は二乗された状態で入力される。1/4πd となっているがdは実際はd^2。
-}

getRadiance :: Light -> Position3 -> Position3
  -> (Double, Maybe Direction3, Radiance)
getRadiance lgt@(Light (Color r g b) f direct shape pow _) lpos pos
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
    --decay = (1.0 / (pi4 * dist)) ** (2.0 * pow)
    cos2 = cos ** (0.5 / pow)
    decay = (1.0 / (pi4 * dist)) ** (2.0 * pow)
    --decay = 1.0
    pw0 = 6.0 * (1.0 - sqrt direct)
    pw  = 1.0 / (10.0 ** pw0 + 1.0)

    --mag = decay * 3.0 * f * (cos ** 0.0)
    mag = decay * 3.0 * f * cos2
    --mag = decay * 3.0 * f * (cos ** (0.5 / pw))
    --mag = (3.0 * f * cos / (pi4 * dist)) ** (-2.0 * pow)
    rad = Radiance (mag * r) (mag * g) (mag * b)
{-
getRadiance l@(PointLight (Color r g b) f _) (d:ds) =
  (Radiance (r * l0) (g * l0) (b * l0)) : getRadiance l ds
  where
    l0 = f / (pi4 * d)
getRadiance l@(ParallelogramLight (Color r g b) f _ _ _ _) (d:ds) =
  (Radiance (r * l0) (g * l0) (b * l0)) : getRadiance l ds
  where
    -- 平面光源は片方向のみ放射するので2倍の出力になる
    l0 = (2 * f) / (pi4 * d)
getRadiance l@(SunLight (Color r g b) f _ _ _ _ _) (_:ds) =
  (Radiance (r * f) (g * f) (b * f)) : getRadiance l ds
-}

