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
, metallicRate
, smoothness
, diffSpec
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
  , ior           :: Color      -- index of refraction
  , diffuseness   :: Double     -- diffuse reflection
  , metallicRate  :: Double
  , smoothness    :: Double
  } deriving Eq

diffSpec :: Material -> Color
diffSpec (Material r _ _ _ _ _ _ _) = r

