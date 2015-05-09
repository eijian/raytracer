--
-- Object
--

module Ray.Object where

import Ray.Geometry
import Ray.Material

data Object = Object Shape Material

initObject :: Shape -> Material -> Object
initObject s m = Object s m

