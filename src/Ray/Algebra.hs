{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

--
-- Ray.Algebra
--

module Ray.Algebra
  ( nearly0
  , o3
  , ex3
  , ey3
  , ez3
  , (+)
  , (-)
  , (*>)
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

nearly0 = 0.00000001 :: Double

class (Show a, Eq a, Additive.C a, Module.C Double a) => BasicMatrix a where
  (/>) :: a -> Double -> Maybe a
  (/>) a s
    | s == 0    = Nothing
    | otherwise = Just ((1 / s) *> a)
  norm :: a -> Double
  (.=.) :: a -> a -> Bool

class (BasicMatrix a) => Vector a where
  (<.>) :: a -> a -> Double
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

elemX :: Vector3 -> Double
elemX (Vector3 ax _ _) = ax
elemY :: Vector3 -> Double
elemY (Vector3 _ ay _) = ay
elemZ :: Vector3 -> Double
elemZ (Vector3 _ _ az) = az

--
-- Position and Direction
--

type Position3  = Vector3
type Direction3 = Vector3

initPos :: Double -> Double -> Double -> Vector3
initPos = Vector3

initDir :: Double -> Double -> Double -> Maybe Direction3
initDir x y z = normalize $ Vector3 x y z

o3  = initPos 0 0 0               -- zero vector
ex3 = fromJust $ initDir 1 0 0    -- unit vector (x axis)
ey3 = fromJust $ initDir 0 1 0    -- unit vector (y axis)
ez3 = fromJust $ initDir 0 0 1    -- unit vector (z axis)

