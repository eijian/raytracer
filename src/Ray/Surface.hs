{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Surface
--

module Ray.Surface (
  Surface (..)
, PhotonBehavior (..)
, reflectance   -- for Simple surface
, albedoDiff   -- for TS surface
, albedoSpec
, bsdf
, densityPower
, initSurfaceSimple
, initSurfaceTS
, one_pi
, reflect
, refract
, sr_half
, photonBehavior
, powerGlossy
, storePhoton
, rough
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Maybe
import           GHC.Generics
import           NumericPrelude

import Ray.Algebra
import Ray.Physics
import Ray.Optics

-- CONSTANTS

--sqpi2 :: Double
--sqpi2 = 2 * pi * pi    -- pi x steradian (2pi) for half sphere

one_pi :: Double
one_pi = 1.0 / pi      -- one of pi (integral of hemisphere)

sr_half :: Double
sr_half = 1.0 / (2.0 * pi)  -- half of steradian

-- Surface type

data Surface =
  None |
  -- My original model
  Simple
  { reflectance   :: !Color
  , specular_refl :: !Color
  , diffuseness   :: !Double
  , metalness     :: !Double
  , smoothness    :: !Double
    -- calculate values
  , density_pow   :: !Double
  } |
  -- Torrance-Sparrow model
  TS
  { albedo_diff :: !Color
  , albedo_spec :: !Color
  , scatterness :: !Double
  , metalness   :: !Double
  , roughness   :: !Double
    -- calculate values
  , density_pow :: !Double
  , alpha       :: !Double
  } |
  DisneyBRDF |
  Brady
  deriving (Eq, Show, Generic)

instance NFData Surface where
  rnf = genericRnf

-- PUBLIC FUNCTIONS

initSurfaceSimple :: Color -> Color -> Double -> Double -> Double
  -> Surface
initSurfaceSimple refl spec diff metal smooth =
  Simple refl spec diff metal smooth (densityPower (1.0 - smooth))

initSurfaceTS :: Color -> Color -> Double -> Double -> Double
  -> Surface
initSurfaceTS al_diff al_spec scat metal rough =
  TS al_diff al_spec scat metal rough (densityPower rough) alpha
  where
    alpha = rough * rough * rough * rough

reflect :: Surface -> Double -> Bool
reflect None _ = False
reflect (Simple _ spec diff _ _ _) cos =
  (diff == 1.0 || (cos == 1.0 && spec == black)) == False
reflect (TS _ alspec _ metal _ _ _) _ =
  if metal == 1.0
    then alspec /= black
    else True
reflect _ _ = False

refract :: Surface -> Double -> Bool
refract None _ = False
refract (Simple _ spec _ _ _ _) cos =
  (cos == 0.0 && spec == white) == False
refract (TS diff _ scat metal _ _ _) _ =
  if metal == 0.0
    then
      if scat < 1.0 && diff /= black
        then True
        else False
    else False
refract _ _ = False

{-

-}

bsdf :: Surface -> Direction3 -> Direction3 -> Direction3 -> Maybe Direction3
  -> Double -> Double -> Radiance -> Radiance -> Radiance
  -> Radiance
bsdf None _ _ _ _ _ _ _ _ _ = radiance0
bsdf (Simple ref spec diff metal _ _) _ _ _ _ cos0 _ di si ti =
  diff           *> (ref <**> (one_pi *> di)) +
    (1.0 - diff) *> (f <**> si + f2 <**> ((1.0 - metal) *> ti))
  where
    f = reflectionIndex spec cos0
    f2 = negateColor f
bsdf (TS aldiff alspec scat metal _ _ _) _ edir rdir _ cos0 _ di si ti =
  i_de + i_mt
  where
    lvec = rdir
    vvec = negate edir
    hvec = fromJust $ normalize (lvec + vvec)
    cos_h = hvec <.> vvec
    f = reflectionIndex alspec cos0
    f2 = negateColor f
    i_de = if metal == 0.0
      then (mulColor aldiff f2) <**> ((scat * one_pi) *> di + (1.0 - scat) *> ti)
      else radiance0
    i_mt = f <**> si
bsdf DisneyBRDF _ _ _ _ _ _ _ _ _ = radiance0
bsdf Brady _ _ _ _ _ _ _ _ _ = radiance0

-- PhotonBehavior

data PhotonBehavior =
    SpecularReflection
  | Absorption
  | DiffuseReflection
  | SpecularTransmission
  deriving Show

photonBehavior :: Surface -> Double -> Wavelength -> IO PhotonBehavior
photonBehavior (TS aldiff alspec scat _ _ _ _) cos wl = do
  r1 <- russianRoulette [f]
  --putStrLn ("r1=" ++ show r1 ++ ", f=" ++ show f)
  if r1 == 1
    then return SpecularReflection
    else do
      r2 <- russianRoulette [selectWavelength wl aldiff]
      --putStrLn ("r2=" ++ show r2 ++ ", diff=" ++ show aldiff)
      if r2 == 0
        then return Absorption
        else do
          r3 <- russianRoulette [scat]
          --putStrLn ("r3=" ++ show r3 ++ ", scat=" ++ show scat)
          if r3 == 0
            then return SpecularTransmission
            else return DiffuseReflection
  where
    f = schlick (selectWavelength wl alspec) cos
photonBehavior _ _ _ = return Absorption

{- |
selectDuffuse
-}

selectDiffuse :: Surface -> Double -> Wavelength -> IO Bool
selectDiffuse (Simple _ _ _ metal _ _) cos wl = do
  r <- russianRoulette [metal]
  if r > 0 then return True else return False
selectDiffuse (TS _ spec _ _ _ _ _) cos wl = do
  let f = schlick (selectWavelength wl spec) cos
  r <- russianRoulette [f]
  if r > 0 then return True else return False
selectDiffuse _ _ _ = return True


storePhoton :: Surface -> Bool
storePhoton (Simple _ _ diff _ _ _) = diff > 0.0
storePhoton (TS _ _ scat metal _ _ _) = metal /= 1.0 && scat /= 0.0
storePhoton _ = True


albedoDiff :: Surface -> Wavelength -> Double
albedoDiff (Simple ref _ _ _ _ _) wl = selectWavelength wl ref
albedoDiff (TS aldiff _ _ _ _ _ _) wl  = selectWavelength wl aldiff
albedoDiff _ _ = 0.0

albedoSpec :: Surface -> Wavelength -> Double
albedoSpec (Simple _ spec _ _ _ _) wl = selectWavelength wl spec
albedoSpec (TS _ alspec _ _ _ _ _) wl = selectWavelength wl alspec
albedoSpec _ _ = 0.0

rough :: Surface -> Double
rough (Simple _ _ _ _ smooth _) = 1.0 - smooth
rough (TS _ _ _ _ rough _ _)  = rough
rough _ = 0.0

powerGlossy :: Surface -> Double
powerGlossy (Simple _ _ _ _ _ pow) = pow
powerGlossy (TS _ _ _ _ _ pow _)   = pow
powerGlossy _ = 0.0

diffSpec :: Surface -> Color
diffSpec (Simple r _ _ _ _ _) = r
diffSpec (TS ad _ _ _ _ _ _)  = ad
diffSpec _ = black

-- UTILITY FUNCTIONS
-- PRIVATE FUNCTIONS

densityPower :: Double -> Double
densityPower rough = 1.0 / (10.0 ** pw + 1.0)
  where
    pw = 6.0 * (1.0 - sqrt rough)

{-
reflectionIndex :: Color -> Double -> Color
reflectionIndex (Color r g b) c =
  Color (r + (1.0 - r) * c2) (g + (1.0 - g) * c2) (b + (1.0 - b) * c2)
  where
    c2 = (1.0 - c) ** 5.0
-}
