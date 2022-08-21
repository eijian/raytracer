--
-- Object
--

module Ray.Object (
  LightObject
, Object (..)
, calcNumPhotons
, initObject
, validLightSpec
) where

import qualified Data.Vector as V

import Ray.Geometry
import Ray.Light
import Ray.Mapper

data Object = Object
  { shape  :: !Shape
  , mapper :: !Mapper
  }
  --deriving (Eq, Show)

type LightObject = Object

initObject :: Shape -> Mapper -> Object
initObject shape mapper = Object shape mapper


{- |
calcNumPhotons: calculation of the number of photons from the light object
-}
calcNumPhotons :: V.Vector LightObject -> Int -> (V.Vector Int, Double)
calcNumPhotons lgts nphoton = (V.map (\x -> round (x / power)) fluxes, power)
  where
    fluxes = V.map flux lgts
    power  = V.sum fluxes / (fromIntegral nphoton)

validLightSpec :: LightObject -> IO (LightSpec, SurfacePoint)
validLightSpec lgt@(Object shp mp) = do
  sfpt <- randomPoint shp
  case lightSpecOnPoint mp sfpt (0.0, 0.0) of
    Nothing      -> validLightSpec lgt
    Just lgtspec -> return (lgtspec, sfpt)

-- PRIVATE FUNCTIONS

{-
flux: calxulatte flux of the light object
  radiosity [W/m^2] x area [m^2] = flux [W]
-}
flux :: LightObject -> Double
flux (Object shp mp) = area * sum radios
  where
    area  = surfaceArea shp
    radios = map (\(m, lgtspec) -> radiosity lgtspec * m) $ lightSpecs mp


