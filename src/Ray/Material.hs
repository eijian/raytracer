--
-- Material
--

module Ray.Material (
  Material (Material)
, reflectance
, transmittance
, specularRefl
, emittance
, ior
, diffuseness
, metalness
, smoothness
, diffSpec
, averageIor
) where

import Ray.Physics
import Ray.Optics

-- CONSTANTS

----
-- Material
----

data Material = Material
  { reflectance   :: Color
  , transmittance :: Color
  , specularRefl  :: Color      -- specular reflectance
  , emittance     :: Radiance
  , ior           :: Radiance   -- index of refraction
  , diffuseness   :: Double     -- diffuse reflection
  , metalness     :: Double
  , smoothness    :: Double
  } deriving Eq

diffSpec :: Material -> Color
diffSpec (Material r _ _ _ _ _ _ _) = r

averageIor :: Material -> Double
averageIor (Material _ _ _ _ (Radiance r g b) _ _ _) = (r + g + b) / 3.0
