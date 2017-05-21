--
-- Material
--

module Ray.Material (
  Material (Material)
, diffSpec
) where

import Ray.Physics

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
  , smoothness    :: Double
  } deriving Eq

diffSpec :: Material -> Color
diffSpec (Material r _ _ _ _ _ _) = r



