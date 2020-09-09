--
-- Object
--

module Ray.Object (
  Object (Object)
, initObject
, material
) where

import Ray.Geometry
import Ray.Material

data Object = Object Shape Material deriving (Eq, Show)

initObject :: Shape -> Material -> Object
initObject s m = Object s m

material :: Object -> Material
material (Object _ m) = m

