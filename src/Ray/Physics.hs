--
-- Physics
--

module Ray.Physics (
  Color (Color)
, black
, Wavelength (Red, Green, Blue)
, initColor
, decideWavelength
, selectWavelength
, negateColor
, scaleColor
, russianRoulette
, reflectionIndex
) where

import System.Random.Mersenne as MT

--
-- Wavelength

data Wavelength = Red | Green | Blue deriving (Show, Read, Enum, Eq)

--
-- Color

data Color = Color !Double !Double !Double

black :: Color
black = Color 0.0 0.0 0.0

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
