{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

--
-- Ray.Algebra
--

module Ray.Algebra
  (module NumericPrelude
  , nearly0
  , o3
  , ex3
  , ey3
  , ez3
--  , (+)
--  , (-)
--  , (*>)
  , (/>)
  , norm
  , (.=.)
  , normalize
  , square
  , (<.>)
  , (<*>)
  , Position3
  , Direction3
  , initPos
  , initDir
  ) where

import Data.Maybe

import NumericPrelude
import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
--import qualified Algebra.NormedSpace.Euclidean as Euclidean
import Test.QuickCheck

nearly0 = 0.00001 :: Double  -- 10 micro meter

class (Show a, Eq a, Additive.C a, Module.C Double a) => BasicMatrix a where
  (/>) :: a -> Double -> Maybe a
  (/>) a s
    | s == 0    = Nothing
    | otherwise = Just ((1 / s) *> a)
  infixl 7 />
  norm :: a -> Double
  (.=.) :: a -> a -> Bool
  infix 4 .=.

class (BasicMatrix a) => Vector a where
  (<.>) :: a -> a -> Double
  infixl 7 <.>
  normalize :: a -> Maybe a
  normalize a = a /> (norm a)
  square :: a -> Double
  square v = v <.> v

data Vector3 = Vector3 Double Double Double

instance Show Vector3 where
  show (Vector3 ax ay az)
    = "[" ++ (show ax) ++ "," ++ (show ay) ++ "," ++ (show az) ++ "]"

instance Eq Vector3 where
  (==) (Vector3 ax ay az) (Vector3 bx by bz)
    = ax == bx && ay == by && az == bz

instance Arbitrary Vector3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Vector3 x y z

instance Additive.C Vector3 where
  zero = Vector3 0 0 0
  (Vector3 ax ay az) + (Vector3 bx by bz)
    = Vector3 (ax + bx) (ay + by) (az + bz)
  (Vector3 ax ay az) - (Vector3 bx by bz)
    = Vector3 (ax - bx) (ay - by) (az - bz)

instance Module.C Double Vector3 where
  s *> (Vector3 x y z) = Vector3 (s * x) (s * y) (s * z)

--instance Euclidean.C Double Vector3 where
--  norm v = sqrt $ normSqr v

instance BasicMatrix Vector3 where
  norm v = sqrt $ square v
  a .=. b = norm (a - b) < nearly0

instance Vector Vector3 where
  (Vector3 ax ay az) <.> (Vector3 bx by bz) = ax * bx + ay * by + az * bz

(<*>) :: Vector3 -> Vector3 -> Vector3
(<*>) (Vector3 ax ay az) (Vector3 bx by bz)
   = Vector3 (ay * bz - by * az) (az * bx - bz * ax) (ax * by - ay * bx)
infix 7 <*>

elemX :: Vector3 -> Double
elemX (Vector3 ax _ _) = ax
elemY :: Vector3 -> Double
elemY (Vector3 _ ay _) = ay
elemZ :: Vector3 -> Double
elemZ (Vector3 _ _ az) = az

-- | Vector3
--
-- Examples:
--
-- >>> let a = Vector3 1 2 3
-- >>> let b = Vector3 3 4 5
-- >>> a + b
-- [4.0,6.0,8.0]
-- >>> a - b
-- [-2.0,-2.0,-2.0]
-- >>> a + b - b
-- [1.0,2.0,3.0]
-- >>> ex3 <*> ey3 == ez3
-- True
-- >>> ey3 <*> ez3 == ex3
-- True
-- >>> ez3 <*> ex3 == ey3
-- True
-- >>> let a = Vector3 (-15.230543143420379) (-969.1420615483364) (-18445.171154285206)
-- >>> let b = Vector3 (-3102.0234662182233) 103.21273514254362 64.53609034791027
-- >>> ((a <*> b) <.> a) < nearly0 
-- True
--
-- prop> \a b -> a + b == (b :: Vector3) + (a :: Vector3)
-- prop> \a b -> a - b == negate ((b :: Vector3) - (a :: Vector3))
-- prop> \a -> a + o3 == (a :: Vector3)
-- prop> \a -> a - o3 == (a :: Vector3)
-- prop> \a b -> a + b - b .=. (a :: Vector3)
-- prop> \a s -> if (s :: Double) == 0 then True else (fromJust ((s *> a) /> s)) .=. (a :: Vector3)
-- prop> \a -> if (a :: Vector3) == o3 then True else (norm $ fromJust $ normalize a) - 1.0 < nearly0
-- prop> \a -> norm (a :: Vector3) == sqrt (a <.> a)
-- prop> \a b -> (a <*> b) <.> a < nearly0  -- cos(90 dig) = 0

--
-- Position and Direction
--

type Position3  = Vector3
type Direction3 = Vector3

initPos :: Double -> Double -> Double -> Vector3
initPos = Vector3

initDir :: Double -> Double -> Double -> Maybe Direction3
initDir x y z = normalize $ Vector3 x y z

-- |
-- initialize direction vector from 2 angles
--   a: latitude (Y axis is origination, toward X axis)
--   b: longitude (X axis is origination, toward Z axis)
initDirFromAngle :: Double -> Double -> Maybe Direction3
initDirFromAngle a b = normalize $ Vector3 x y z
  where
    sina = sin a
    x = sina * cos b
    y = cos a
    z = sina * sin b

o3  = initPos 0 0 0               -- zero vector
ex3 = fromJust $ initDir 1 0 0    -- unit vector (x axis)
ey3 = fromJust $ initDir 0 1 0    -- unit vector (y axis)
ez3 = fromJust $ initDir 0 0 1    -- unit vector (z axis)


-- |
-- positional vector and directional vector
--
-- >>> initDir 0 0 0
-- Nothing
-- >>> let rt3 = 1.0 / sqrt 3.0
-- >>> fromJust (initDir 1 1 1) == Vector3 rt3 rt3 rt3
-- True
-- 
-- prop> \a b -> norm (fromJust $ initDirFromAngle a b) - 1.0 < nearly0

