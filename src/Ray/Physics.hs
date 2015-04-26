--
-- Physics
--

module Physics
where

import Ray.Algebra
import Ray.Geometry

data Wavelength = Red | Green | Blue deriving (Show, Enum)

data Photon = (Wavelength, Ray)

initPhoton :: Wavelength -> Ray -> Photon
initPhoton l r = (l, r)

--
-- light

data Color = Color Double Double Double deiving (Show, Eq)

instance Show Color where
  show (Color r g b) = "[" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "]"

instance Eq Color where
  (Color ar ag ab) == (Color br bg bb) = (ar == br) && (ag == bg) && (ab == bb)

initColor :: Double -> Double -> Double -> Color
initColor r g b = Color (r / mag) (g / mag) (b / mag)
  where
    mag = r + g + b

decideWavelength :: Color -> Double -> Maybe Wavelength
decideWavelength (Color r g b) p
  | p < 0.0 || p > 1.0 = Nothing
  | p < r              = Just Red
  | p < r + g          = Just Green
  | otherwise          = Just Blue

--

type Flux = Double

class Light a where
  flux :: a -> Flux
  generatePhoton :: a -> IO Photon
  

data PointLight = PointLight Color Flux Position3 deriving Show

instance Show PointLight where
  show (PointLight c f p) = "[" ++ show c ++ "," ++ show f ++ "," ++ show p ++ "]"



