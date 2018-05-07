--
-- VectorTest: test of Vector type definitions
--

module VectorTest where

import Test.QuickCheck

nearlyZero = 0.00000001 :: Double

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
-- vector addition
--
-- >>> vadd (Vector3 1 2 3) (Vector3 4 5 6)
-- [5.0,7.0,9.0]
--

vadd :: Vector3 -> Vector3 -> Vector3
vadd (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax + bx) (ay + by) (az + bz)

-- |
-- vector substract
--
-- >>> vsub (Vector3 1 2 3) (Vector3 4 5 6)
-- [-3.0,-3.0,-3.0]
--

vsub :: Vector3 -> Vector3 -> Vector3
vsub (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax - bx) (ay - by) (az - bz)

-- |
-- vector scaling
--
-- >>> vscale 4.0 (Vector3 1.1 2.1 3.1)
-- [4.4,8.4,12.4]
--

vscale :: Double -> Vector3 -> Vector3
vscale s (Vector3 ax ay az) = Vector3 (s * ax) (s * ay) (s * az)

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
-- QuickCheck
--
-- prop> \a b -> vsub (vadd a b) b .=. a
-- prop> \a b -> vadd (vsub a b) b .=. a
-- prop> \a s -> elemX (vscale s a) == s * elemX a
-- prop> dot (Vector3 ax ay az) (Vector3 bx by bz) == ax * bx + ay * by + az * bz
-- prop> elemX (cross (Vector3 ax ay az) (Vector3 bx by bz)) == ay * bz - az * by
-- prop> elemY (cross (Vector3 ax ay az) (Vector3 bx by bz)) == az * bx - ax * bz
-- prop> elemZ (cross (Vector3 ax ay az) (Vector3 bx by bz)) == ax * by - ay * bx


