--
-- Material
--

module Ray.Material where

import Ray.Physics

-- CONSTANTS

----
-- Material
----

data Material = Material Color  -- diffuse specular ratio
                deriving Eq

diffSpec :: Material -> Color
diffSpec (Material d) = d


