{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

---
--- Mapper
---

module Ray.Mapper (
  Mapper (..)
, SurfaceChar
, lightSpecOnPoint
, lightSpecs
, surfaceCharOnPoint
) where

import           Data.Maybe
import           NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Material
import Ray.Surface


type SurfaceChar = (Material, Surface)

data Mapper =
  Solid
  { sfchar :: !SurfaceChar
  }
  |
  Checker
  { sfchar1 :: !SurfaceChar
  , sfchar2 :: !SurfaceChar
  , mag     :: !Double
  }
  deriving (Eq)

instance Show Mapper where
  show (Solid sfchar) = "Solid " ++ (show sfchar)
  show (Checker sfchar1 sfchar2 mag) = "Checker " ++ (show sfchar1) ++ ", " ++ (show sfchar2) ++ ", " ++ show mag

-- mapping functions
initSolid :: SurfaceChar -> Mapper
initSolid sc = Solid sc

initChecker :: SurfaceChar -> SurfaceChar -> Double -> Mapper
initChecker sc1 sc2 scale = Checker sc1 sc2 scale


surfaceCharOnPoint :: Mapper -> SurfacePoint -> Vector2 -> SurfaceChar
surfaceCharOnPoint (Solid sc) _ _ = sc
surfaceCharOnPoint (Checker sc1 sc2 mag) ((Vector3 x _ z), _) _ =
  if sc >= 0 then sc1 else sc2
  where
    x' = x * pi * mag
    z' = z * pi * mag
    sc = sin x' * sin z'

lightSpecOnPoint :: Mapper -> SurfacePoint -> Vector2 -> Maybe LightSpec
lightSpecOnPoint mp sp uv = elight sf
  where
    (_, sf) = surfaceCharOnPoint mp sp uv

lightSpecs :: Mapper -> [(Double, LightSpec)]
lightSpecs (Solid (_, sf)) = case elight sf of
  Nothing      -> []
  Just lgtspec -> [(1.0, lgtspec)]
lightSpecs (Checker (_, sf1) (_, sf2) _) = zip (repeat 0.5) lss
  where
    lss = catMaybes [elight sf1, elight sf2]


---
