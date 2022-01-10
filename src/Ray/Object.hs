--
-- Object
--

module Ray.Object (
  Object (Object)
, initObject
) where

import Ray.Geometry
import Ray.Material
import Ray.Surface

data Object = Object Shape Material Surface deriving (Eq, Show)

initObject :: Shape -> Material -> Surface -> Object
initObject shape mate surf= Object shape mate surf

