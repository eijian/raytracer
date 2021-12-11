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

data Object = Object Shape Material deriving (Eq, Show)

initObject :: Shape -> Material -> Object
initObject shape mate = Object shape mate

