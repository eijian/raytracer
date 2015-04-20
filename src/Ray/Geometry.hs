--
-- Geometry
--

module Geometry
where

import Ray.Algebra

type Ray = (Position, Direction)

initRay :: Position -> Vector3 -> Maybe Ray
initRay p v
  | v == o3 = Nothing
  | otherwise = Just (p, v')
  where
    v' = normalize v

target :: Double -> Ray -> Position
target t (p, d) = p madd (mscale t d)

getPos :: Ray -> Position
getPos = fst

getDir :: Ray -> Direction
getDir = snd


-- Shape

class Shape a where
  getNormal :: a -> Direction


