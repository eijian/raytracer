--
-- Object
--

module Ray.Object (
  Object (Object)
, initObject
) where

import Ray.Geometry
import Ray.Material

data Object = Object Shape Material deriving (Eq, Show)

initObject :: Shape -> Material -> Object
initObject s m = Object s m

