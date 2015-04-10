

--
-- Algebra:
--

module Ray.Algebra  where


----
-- CLASSES
----

class (Show a, Eq a) => Matrix a where
  madd :: a -> a -> a
  msub :: a -> a -> a
  mscale :: Double -> a -> a
  mdiv :: a -> Double -> Maybe a
  mdiv a s
    | s == 0   = Nothing
    | otherwise = Just (mscale (1 / s) a)
  norm :: a -> Double

class (Matrix a) => Vector a where
  dot :: a -> a -> Double
  normal :: a -> Maybe a
  normal a = a `mdiv` (norm a)
  square :: a -> Double
  square a = a `dot` a

newtype Vector2 = Vector2 (Double, Double) deriving Vector

instance Matrix Vector2 where
  madd (ax, ay) (bx, by) = (ax + ay, bx + by)
  msub (ax, ay) (bx, by) = (ax - ay, bx - by)




