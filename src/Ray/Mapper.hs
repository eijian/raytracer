{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

---
--- Mapper
---

module Ray.Mapper (
  Mapper
, SurfaceChar
, checkerMapper
, uniMapper
) where

import           NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Material
import Ray.Surface


type SurfaceChar = (Material, Surface)

type Mapper = SurfacePoint -> Vector2 -> SurfaceChar

-- mapping functions

uniMapper :: SurfaceChar
  -> SurfacePoint -> Vector2 -> SurfaceChar
uniMapper sc _ _ = sc

checkerMapper :: SurfaceChar -> SurfaceChar
  -> SurfacePoint -> Vector2 -> SurfaceChar
checkerMapper sc1 sc2 ((Vector3 x _ z), _) _ = if sc >= 0 then sc1 else sc2
  where
    x' = x * pi * 2.5
    z' = z * pi * 2.5
    sc = sin x' * sin z'

---
