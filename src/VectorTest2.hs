--
-- VectorTest: test of Vector type definitions
--

module VectorTest2 where

import Test.QuickCheck

nearlyZero = 0.00000001 :: Double

type Vector3 = (Double, Double, Double)

{-
instance Show Vector3 where
  show (ax, ay, az) = "[" ++ (show ax) ++ "," ++ (show ay) ++ "," ++ (show az) ++ "]"

instance Eq Vector3 where
  (==) (ax, ay, az) (bx, by, bz) = ax == bx && ay == by && az == bz
-}

{-
instance Arbitrary Vector3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ (x, y, z)
-}

-- |
-- initialize Vector
--

initVec :: Double -> Double -> Double -> Vector3
initVec x y z = (x, y, z)

-- |
-- nearly equal Zero
--

(.=.) :: Vector3 -> Vector3 -> Bool
(.=.) (ax, ay, az) (bx, by, bz) = abs (ax - bx) < nearlyZero &&
                                  abs (ay - by) < nearlyZero &&
                                  abs (az - bz) < nearlyZero

-- |
-- element X axis of vector
--
-- >>> elemX (1, 2, 3)
-- 1.0
-- >>> elemY (1, 2, 3)
-- 2.0
-- >>> elemZ (1, 2, 3)
-- 3.0
--

elemX :: Vector3 -> Double
elemX (ax, _, _) = ax
elemY :: Vector3 -> Double
elemY (_, ay, _) = ay
elemZ :: Vector3 -> Double
elemZ (_, _, az) = az

-- |
-- vector addition
--
-- >>> vadd (1, 2, 3) (4, 5, 6)
-- (5.0,7.0,9.0)
--

vadd :: Vector3 -> Vector3 -> Vector3
vadd (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)

-- |
-- vector substract
--
-- >>> vsub (1, 2, 3) (4, 5, 6)
-- (-3.0,-3.0,-3.0)
--

vsub :: Vector3 -> Vector3 -> Vector3
vsub (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)

-- |
-- vector scaling
--
-- >>> vscale 4.0 (1.1, 2.1, 3.1)
-- (4.4,8.4,12.4)
--

vscale :: Double -> Vector3 -> Vector3
vscale s (ax, ay, az) = (s * ax, s * ay, s * az)

-- |
-- dot vector
--

dot :: Vector3 -> Vector3 -> Double
dot (ax, ay, az) (bx, by, bz) = ax * bx + ay * by + az * bz

-- |
-- cross vector
--

cross :: Vector3 -> Vector3 -> Vector3
cross (ax, ay, az) (bx, by, bz) = (ay * bz - by * az, az * bx - bz * ax, ax * by - ay * bx)

-- |
-- QuickCheck
--
-- prop> \a b -> vsub (vadd a b) b .=. a
-- prop> \a b -> vadd (vsub a b) b .=. a
-- prop> \a s -> elemX (vscale s a) == s * elemX a
-- prop> dot (ax, ay, az) (bx, by, bz) == ax * bx + ay * by + az * bz
-- prop> elemX (cross (ax, ay, az) (bx, by, bz)) == ay * bz - az * by
-- prop> elemY (cross (ax, ay, az) (bx, by, bz)) == az * bx - ax * bz
-- prop> elemZ (cross (ax, ay, az) (bx, by, bz)) == ax * by - ay * bx


