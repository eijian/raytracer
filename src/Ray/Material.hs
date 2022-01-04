{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

--
-- Material
--

module Ray.Material (
  Material (Material)
, PhotonBehavior (..)
, albedoDiff
, albedoSpec
, averageIor
, initMaterial
, ior
, metalness
, photonBehavior
, reflect
, refract
, storePhoton
, transmittance
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           GHC.Generics
  
import Ray.Algebra
import Ray.Physics
import Ray.Optics

-- CONSTANTS

----
-- Material
----

data Material = Material
  { albedoDiff   :: !Color
  , scatterness   :: !Double
  , metalness     :: !Double
  , transmittance :: !Color
  , ior           :: !Color      -- index of refraction
  -- calculated value
  , albedoSpec   :: !Color
  } deriving (Eq, Show, Generic)

instance NFData Material where
  rnf = genericRnf

initMaterial :: Color -> Double -> Double -> Color -> Color -> Maybe Color
  -> Material
initMaterial aldiff scat metal tran ior@(Color ir ig ib) alspec =
  case alspec of
    Nothing ->
      let alspec = Color (eta ir) (eta ig) (eta ib)
      in Material aldiff scat metal tran ior alspec
    Just alspec -> Material aldiff scat metal tran ior alspec

eta :: Double -> Double
eta x = n * n
  where
    n = (1.0 - x) / (1.0 + x)

averageIor :: Material -> Double
averageIor (Material _ _ _ _ (Color r g b) _) = (r + g + b) / 3.0

reflect :: Material -> Double -> Bool
reflect (Material _ _ metal _ _ as) _ = if metal == 1.0
  then True
  else as /= black

refract :: Material -> Bool
refract (Material ad scat metal _ _ _) = if metal /= 1.0
    then if scat < 1.0 && ad /= black then True else False
    else False

-- PhotonBehavior

data PhotonBehavior =
    SpecularReflection
  | Absorption
  | DiffuseReflection
  | SpecularTransmission
  deriving Show

photonBehavior :: Material -> Double -> Wavelength -> IO PhotonBehavior
photonBehavior (Material ad scat _ _ _ as) cos wl = do
  r1 <- russianRouletteBinary f
  --putStrLn ("r1=" ++ show r1 ++ ", f=" ++ show f)
  if r1 == True
    then return SpecularReflection
    else do
      r2 <- russianRouletteBinary (selectWavelength wl ad)
      --putStrLn ("r2=" ++ show r2 ++ ", diff=" ++ show aldiff)
      if r2 == False
        then return Absorption
        else do
          r3 <- russianRouletteBinary scat
          --putStrLn ("r3=" ++ show r3 ++ ", scat=" ++ show scat)
          if r3 == False
            then return SpecularTransmission
            else return DiffuseReflection
  where
    f = schlick (selectWavelength wl as) cos

{- |
selectDuffuse
-}

selectDiffuse :: Material -> Double -> Wavelength -> IO Bool
selectDiffuse (Material _ _ _ _ _ as) cos wl = do
  let f = schlick (selectWavelength wl as) cos
  russianRouletteBinary f

storePhoton :: Material -> Bool
storePhoton (Material _ scat metal _ _ _) = metal /= 1.0 && scat /= 0.0

{-
diffSpec :: Surface -> Color
diffSpec (TS ad _ _ _ _ _ _)  = ad
diffSpec _ = black
-}

-- PRIVATE FUNCTIONS

