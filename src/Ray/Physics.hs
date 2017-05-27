--
-- Physics
--

module Ray.Physics (
  Color (Color)
, black
, Wavelength (Red, Green, Blue)
, initColor
, decideWavelength
, russianRoulette
) where

import System.Random.Mersenne as MT

--
-- Wavelength

data Wavelength = Red | Green | Blue deriving (Show, Read, Enum, Eq)

--
-- Color

data Color = Color Double Double Double

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

{- |
russianRoulette

>>> russianRoulette Red [(Color 0.5 0.0 0.0)]
0

-}

russianRoulette :: Wavelength -> [Color] -> IO Int
russianRoulette wl cs = do
  r <- MT.randomIO :: IO Double
  return $ rr wl cs 0.0 r (length cs)

rr :: Wavelength -> [Color] -> Double -> Double -> Int -> Int
rr _ [] _ _ len = len
rr wl (c:cs) c0 r len
  | r < c'    = len
  | otherwise = rr wl cs c' r (len - 1)
  where
    c' = c0 + selWl wl c
    selWl :: Wavelength -> Color -> Double
    selWl Red   (Color r _ _) = r
    selWl Green (Color _ g _) = g
    selWl Blue  (Color _ _ b) = b

