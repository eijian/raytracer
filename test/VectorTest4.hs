--
-- VectorTest: test of Vector type definitions
--

module VectorTest3 where

import Test.QuickCheck

nearlyZero = 0.00000001 :: Double


class Vector a where
  vadd :: a -> a -> a
  vsub :: a -> a -> a
  vscale :: Double -> a -> a
--  vdiv   :: a -> Double -> Maybe a
--  vdiv a s
--    |s == 0  = Nothing
--    |otherwise = Just (vscale (1.0 / s) a)
--  norm :: a -> Double
  dot  :: a -> a -> Double
--  normalize :: a -> Maybe a
--  normalize a = a `vdiv` (norm a)
--  square :: a -> Double
--  square a = a `dot` a
  (.=.) :: a -> a -> Bool

data Vector3 = Vector3 Double Double Double

instance Show Vector3 where
  show (ax, ay, az) = "[" ++ (show ax) ++ "," ++ (show ay) ++ "," ++ (show az) ++ "]"

instance Eq Vector3 where
  (==) (ax, ay, az) (bx, by, bz) = ax == bx && ay == by && az == bz

instance Arbitrary Vector3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ (x, y, z)

-- |
-- initialize Vector
--
initVec :: Double -> Double -> Double -> Vector3
initVec x y z = Vector3 x y z

-- |
-- element X axis of vector
--
-- >>> elemX $ initVec 1.0 2.0 3.0
-- 1.0
-- >>> elemY $ initVec 1.0 2.0 3.0
-- 2.0
-- >>> elemZ $ initVec 1.0 2.0 3.0
-- 3.0
--
elemX :: Vector3 -> Double
elemX (Vector3 ax _ _) = ax
elemY :: Vector3 -> Double
elemY (Vector3 _ ay _) = ay
elemZ :: Vector3 -> Double
elemZ (Vector3 _ _ az) = az

-- |
-- cross vector
--
cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 ax ay az:_) (bx:by:bz:_) = Vector3 [ay * bz - by * az, az * bx - bz * ax, ax * by - ay * bx]

instance Vector Vector3 where
  -- |
  -- nearly equal Zero
  --
  (.=.) :: Vector3 -> Vector3 -> Bool
  (.=.) a b = not $ any (> nearlyZero) (zipWith (-) a b)

  -- |
  -- vector addition
  --
  -- >>> vadd (initVec 1 2 3) (initVec 4 5 6)
  -- [5.0,7.0,9.0]
  --
  vadd :: Vector3 -> Vector3 -> Vector3
  vadd a b = zipWith (+) a b

  -- |
  -- vector substract
  --
  -- >>> vsub (initVec 1 2 3) (initVec 4 5 6)
  -- [-3.0,-3.0,-3.0]
  --
  vsub :: Vector3 -> Vector3 -> Vector3
  vsub a b = zipWith (-) a b

  -- |
  -- vector scaling
  --
  -- >>> vscale 4.0 (initVec 1.1 2.1 3.1)
  -- [4.4,8.4,12.4]
  --
  vscale :: Double -> Vector3 -> Vector3
  vscale s a = map (* s) a

  -- |
  -- dot vector
  --
  dot :: Vector3 -> Vector3 -> Double
  dot a b = sum $ zipWith (*) a b

-- |
-- QuickCheck
--
-- prop> \a b -> vsub (vadd a b) b .=. a
-- prop> \a b -> vadd (vsub a b) b .=. a
-- -- prop> \a s -> elemX (vscale s a) == s * elemX a
-- prop> dot [ax, ay, az] [bx, by, bz] == ax * bx + ay * by + az * bz
-- prop> elemX (cross [ax, ay, az] [bx, by, bz]) == ay * bz - az * by
-- prop> elemY (cross [ax, ay, az] [bx, by, bz]) == az * bx - ax * bz
-- prop> elemZ (cross [ax, ay, az] [bx, by, bz]) == ax * by - ay * bx


