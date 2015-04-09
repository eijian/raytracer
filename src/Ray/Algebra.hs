--
-- Ray.Algebra
--

module Ray.Algebra where

import Data.Maybe

nearlyZero = 0.00000001 :: Double

class (Show a, Eq a) => BasicMatrix a where
  madd :: a -> a -> a
  msub :: a -> a -> a
  mscale :: Double -> a -> a
  mdiv   :: a -> Double -> a
  norm :: a -> Double
  nealyEqual :: a -> a -> Bool

class (BasicMatrix a) => Vector a where
  dot :: a -> a -> Double
  normalize :: a -> Maybe a
  normalize a = a `mdiv` (norm a)
  square :: a -> Double
  square a = a `dot` a

data Vector3 = Vector3 Double Double Double

instance Show Vector3 where
  show (Vector3 ax ay az) = "[" ++ (show ax) ++ "," ++ (show ay) ++ "," ++ (show az) ++ "]"

instance Eq Vector3 where
  (==) (Vector3 ax ay az) (Vector3 bx by bz) = ax == bx && ay == by && az == bz

instance Arbitrary Vector3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Vector3 x y z


instance BasicMatrix Vector3 where
  -- |
  -- vector addition
  --
  -- >>> madd (Vector3 1 2 3) (Vector3 4 5 6)
  -- [5.0,7.0,9.0]
  --
  madd :: Vector3 -> Vector3 -> Vector3
  madd (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax + bx) (ay + by) (az + bz)

  -- |
  -- vector substract
  --
  -- >>> msub (Vector3 1 2 3) (Vector3 4 5 6)
  -- [-3.0,-3.0,-3.0]
  --
  msub :: Vector3 -> Vector3 -> Vector3
  msub (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax - bx) (ay - by) (az - bz)

  -- |
  -- vector scaling
  --
  -- >>> mscale 4.0 (Vector3 1.1 2.1 3.1)
  -- [4.4,8.4,12.4]
  --
  mscale :: Double -> Vector3 -> Vector3
  mscale s (Vector3 x y z) = Vector3 (s * x) (s * y) (s * z)

  -- |
  -- vector scalar division
  --
  mdiv :: Vector3 -> Vector3 -> Vector3
  mdiv (Vector3 x y z) s = Vector3 (x / s) (y / s) (z / s)

  -- |
  -- dot vector
  --
  dot :: Vector3 -> Vector3 -> Double
  dot (Vector3 ax ay az) (Vector3 bx by bz) = ax * bx + ay * by + az * bz

-- |
-- cross vector
--

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ay * bz - by * az) (az * bx - bz * ax) (ax * by - ay * bx)

-- |
-- initialize Vector
--

initVec :: Double -> Double -> Double -> Vector3
initVec x y z = Vector3 x y z

-- |
-- nearly equal Zero
--

(.=.) :: Vector3 -> Vector3 -> Bool
(.=.) (Vector3 ax ay az) (Vector3 bx by bz) = abs (ax - bx) < nearlyZero &&
                                              abs (ay - by) < nearlyZero &&
                                              abs (az - bz) < nearlyZero

-- |
-- element X axis of vector
--
-- >>> elemX (Vector3 1 2 3)
-- 1.0
-- >>> elemY (Vector3 1 2 3)
-- 2.0
-- >>> elemZ (Vector3 1 2 3)
-- 3.0
--

elemX :: Vector3 -> Double
elemX (Vector3 ax _ _) = ax
elemY :: Vector3 -> Double
elemY (Vector3 _ ay _) = ay
elemZ :: Vector3 -> Double
elemZ (Vector3 _ _ az) = az

-- |
-- QuickCheck
--
-- prop> \a b -> vsub (vadd a b) b .=. a
-- prop> \a b -> vadd (vsub a b) b .=. a
-- prop> \a s -> elemX (vscale s a) == s * elemX a
-- prop> dot (Vector3 ax ay az) (Vector3 bx by bz) == ax * bx + ay * by + az * bz
-- prop> elemX (cross (Vector3 ax ay az) (Vector3 bx by bz)) == ay * bz - az * by
-- prop> elemY (cross (Vector3 ax ay az) (Vector3 bx by bz)) == az * bx - ax * bz
-- prop> elemZ (cross (Vector3 ax ay az) (Vector3 bx by bz)) == ax * by - ay * bx


