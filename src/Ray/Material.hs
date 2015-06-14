--
-- Material
--

module Ray.Material where

import Ray.Physics

data Material = Material Double Double Double deriving Eq -- diffuse specular ratio

