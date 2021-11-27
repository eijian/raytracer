{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

--
-- Material
--

module Ray.Material (
  Material (Material)
, reflectance
, transmittance
, specularRefl
, emittance
, ior
, diffuseness
, metalness
--, smoothness
, averageIor
, surface
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           GHC.Generics
  
import Ray.Physics
import Ray.Optics
import Ray.Surface hiding (diffuseness, metalness, reflectance)

-- CONSTANTS

----
-- Material
----

data Material = Material
  { emittance     :: Radiance
  , transmittance :: Color
  , ior           :: Color      -- index of refraction
  , surface       :: Surface
  } deriving (Eq, Show, Generic)

instance NFData Material where
  rnf = genericRnf

averageIor :: Material -> Double
averageIor (Material _ _ (Color r g b) _) = (r + g + b) / 3.0

diffuseness :: Material -> Double
diffuseness (Material _ _ _ s) = case s of
  (Simple _ _ diff _ _ _) -> diff
  (TS _ _ _ _ rough _ _)  -> rough
  _                       -> 0.0

metalness :: Material -> Double
metalness (Material _ _ _ s) = case s of
  (Simple _ _ _ metal _ _) -> metal
  (TS _ _ _ metal _ _ _)   -> metal
  _                        -> 0.0

reflectance :: Material -> Color
reflectance (Material _ _ _ s) = case s of
  (Simple refl _ _ _ _ _) -> refl
  (TS adiff _ _ _ _ _ _)  -> adiff
  _                       -> black

specularRefl :: Material -> Color
specularRefl (Material _ _ _ s) = case s of
  (Simple _ spec _ _ _ _) -> spec
  (TS _ spec _ _ _ _ _)   -> spec
  _                       -> black


