{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

--{-# LANGUAGE BangPatterns #-}

--
-- Material
--

module Ray.Material (
  Material (..)
, PhotonBehavior (..)
--, albedoDiff
--, albedoSpec
, averageIor
, initMaterial
--, ior
--, metalness
, photonBehavior
, reflect
, refract
, scatter
--, scatterness
, storePhoton
--, transmittance
) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics
  
import Ray.Algebra
import Ray.Physics
import Ray.Optics

-- CONSTANTS

----
-- Material
----

data Material = Material
  { albedoDiff    :: !Color
  , scatterness   :: !Double
  , metalness     :: !Double
  , transmittance :: !Color
  , ior           :: !Color      -- index of refraction
  , albedoSpec    :: !Color
  } deriving (Eq, Show, Generic)

instance NFData Material where
  rnf :: Material -> ()
  rnf = genericRnf

initMaterial :: Color -> Double -> Double -> Color -> Color -> Maybe Color
  -> Material
initMaterial aldiff scat metal tran ior@(Color ir ig ib) alspec =
  Material aldiff scat metal tran ior alspec'
  where
    alspec' = case alspec of
      Nothing -> Color (eta ir) (eta ig) (eta ib)
      Just as -> as
    -- eta: 反射率を求める。物体の屈折率をn、真空の屈折率をn0(=1.0)0とした時
    --  反射率r = {(n0 - n)/(n0 + n)}^2
    --  が成り立つ。
    eta :: Double -> Double
    eta x = n * n
      where
        n = (1.0 - x) / (1.0 + x)

averageIor :: Material -> Double
averageIor (Material _ _ _ _ (Color r g b) _) = (r + g + b) / 3.0

reflect :: Material -> Double -> Bool
reflect (Material _ _ metal _ _ alspec) _ = (metal == 1.0) || (alspec /= black)

refract :: Material -> Bool
refract (Material aldiff scat metal _ _ _) = if metal /= 1.0
    then if scat < 1.0 && aldiff /= black
      then True
      else False
    else False

scatter :: Material -> Bool
scatter (Material _ scat _ _ _ _) = scat /= 0.0

-- PhotonBehavior

data PhotonBehavior =
    SpecularReflection
  | Absorption
  | DiffuseReflection
  | SpecularTransmission
  deriving Show

photonBehavior :: Material -> Double -> Wavelength -> IO PhotonBehavior
photonBehavior (Material aldiff scat _ _ _ alspec) cos wl = do
  let
    f = fresnelReflectance (selectWavelength wl alspec) cos
  r1 <- russianRouletteBinary f
  --putStrLn ("r1=" ++ show r1 ++ ", f=" ++ show f)
  if r1
    then return SpecularReflection
    else do
      {-
      r2<- russianRouletteBinary scat
      if r2 == False
        then return SpecularTransmission
        else do
          r3 <- russianRouletteBinary (selectWavelength wl aldiff)
          if r3 == False
            then return Absorption
            else return DiffuseReflection
      -}
      r2 <- russianRouletteBinary (selectWavelength wl aldiff)
      --putStrLn ("r2=" ++ show r2 ++ ", diff=" ++ show aldiff)
      if not r2
        then return Absorption
        else do
          r3 <- russianRouletteBinary scat
          --putStrLn ("r3=" ++ show r3 ++ ", scat=" ++ show scat)
          if not r3
            then return SpecularTransmission
            else return DiffuseReflection

{- |
selectDuffuse

selectDiffuse :: Material -> Double -> Wavelength -> IO Bool
selectDiffuse (Material _ _ _ _ _ as) cos wl = do
  let f = schlick (selectWavelength wl as) cos
  russianRouletteBinary f
-}

{- |
storePhoton:

-}

storePhoton :: Material -> Bool
storePhoton (Material _ scat metal _ _ _) = metal /= 1.0 && scat /= 0.0

-- PRIVATE FUNCTIONS

