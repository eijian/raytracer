--
-- Object
--

module Ray.Object (
  Object (Object)
, initObject
) where

import Ray.Geometry
import Ray.Mapper
--import Ray.Material
--import Ray.Surface

data Object = Object Shape Mapper
  --deriving (Eq, Show)

initObject :: Shape -> Mapper -> Object
initObject shape mapper = Object shape mapper

