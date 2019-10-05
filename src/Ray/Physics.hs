{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

--
-- Physics
--

module Ray.Physics (
  Color (Color)
, black
, white
, Wavelength (Red, Green, Blue)
, initColor
, normalizeColor
, decideWavelength
, selectWavelength
, negateColor
, scaleColor
, addColor
, russianRoulette
, reflectionIndex
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           GHC.Generics
import           System.Random.Mersenne as MT

--
-- Wavelength

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

reflectionIndex :: Color -> Double -> Color
reflectionIndex (Color r g b) c =
  Color (r + (1-r) * c') (g + (1-g) * c') (b + (1-b) * c')
  where
    c' = (1.0 - c) ** 5.0
