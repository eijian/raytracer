{-# LANGUAGE NoImplicitPrelude #-}

--
-- Light
--

module Ray.Light (
  Light (PointLight, ParallelogramLight, SunLight)
, flux
, generatePhoton
, getDirection
, getRadiance
) where

--import System.Random
import System.Random.Mersenne as MT
import Data.Maybe
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Physics
import Ray.Optics

type Flux = Double

data Light =
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

instance Show Light where
  show (PointLight c f p) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "]"
  show (ParallelogramLight c f p n d1 d2) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "," ++ show n ++ "," ++ show d1 ++ "," ++ show d2 ++ "]"
  show (SunLight c f p n d1 d2 d0) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "," ++ show n ++ "," ++ show d1 ++ "," ++ show d2 ++ "," ++ show d0 ++ "]"

flux :: Light -> Flux
flux (PointLight _ f _) = f
flux (ParallelogramLight _ f _ _ _ _) = f
flux (SunLight _ f _ _ _ _ _) = f

generatePhoton :: Light -> IO Photon
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

pi4 :: Double
pi4 = 4 * pi -- for decay by distance (1/ 4pi) 

paraDiv :: Double
paraDiv = 0.2
ts :: [Double]
ts = [0.1, 0.3, 0.5, 0.7, 0.9]
tss :: [(Double, Double)]
tss = [(x, y) | x <- ts, y <- ts]

getDirection :: Light -> Position3 -> [Direction3]
getDirection (PointLight _ _ lp) p = [lp - p]
getDirection (ParallelogramLight _ _ lp n d1 d2) p =
  filter (\d -> n <.> d < 0.0) $ map (\(tx, ty) -> genPos tx ty - p) tss
  where
    genPos :: Double -> Double -> Position3
    genPos tx ty = lp + tx *> d1 + ty *> d2
getDirection (SunLight _ _ lp n d1 d2 dt) p
  | cos0 > 0.0     = []
  | res == Nothing = []
  | otherwise      = [t *> dt']
  where
    d = lp - p
    cos0 = n <.> d
    dt' = negate dt
    res = methodMoller 2.0 lp d1 d2 p dt'
    (_, _, t) = fromJust res

getRadiance :: Light -> [Double] -> [Radiance]
getRadiance _ [] = [radiance0]
getRadiance l@(PointLight (Color r g b) f _) (d:ds) =
  (Radiance (r * l0) (g * l0) (b * l0)) : getRadiance l ds
  where
    l0 = f / (pi4 * d)
getRadiance l@(ParallelogramLight (Color r g b) f _ _ _ _) (d:ds) =
  (Radiance (r * l0) (g * l0) (b * l0)) : getRadiance l ds
  where
    -- 平面光源は片方向のみ放射するので2倍の出力になる
    l0 = (2 * f * paraDiv * paraDiv) / (pi4 * d)
getRadiance l@(SunLight (Color r g b) f _ _ _ _ _) (_:ds) =
  (Radiance (r * f) (g * f) (b * f)) : getRadiance l ds
