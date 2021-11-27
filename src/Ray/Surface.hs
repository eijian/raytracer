{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

--
-- Surface
--

module Ray.Surface (
  Surface (..)
, reflectance   -- for Simple surface
, albedo_diff   -- for TS surface
, initSurfaceSimple
, initSurfaceTS
, store_photon
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           GHC.Generics
  
import Ray.Physics
import Ray.Optics

-- CONSTANTS

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
bsdf :: Surface -> Direction3 -> Direction3 -> Direction3 -> Maybe Direction3
  -> Double -> Double -> Radiance -> Radiance -> Radiance
  -> Radiance
bsdf Nothing _ _ _ _ _ _ _ _ _ = radiance0
bsdf (Simple ref spec diff metal _ _) _ _ _ _ cos0 _ di si ti =
  diff         * (ref * ONE_PI * di) +
  (1.0 - diff) * (f * si + (1.0 - metal) * f2 * ti)
  where
    f = reflectionIndex spec cos0
    f2 = -f
bsdf (TS diff spec scat _ _ _ _) _ edir rdir _ cos0 _ di si ti =
  i_de + i_mt
  where
    lvec = rdir
    vvec = -edir
    hvec = normalize (lvec + vvec)
    cos_h = hvec <.> vvec
    f = reflectionIndex spec cos0
    f2 = -f
    i_de = if metalness == 0.0
      then f2 * diff * (scat * ONE_PI * di + (1.0 - scat) * ti)
    i_mt = f * si
bsdf DisneyBRDF _ _ _ _ _ _ _ _ _ = radiance0
bsdf Brady _ _ _ _ _ _ _ _ _ = radiance0
-}

-- OUT: dir  next ray direction. if dir is None, the photon is absorbed.
--      T/F  true = reflection, false = rafraction
{-
nextDirection :: Surface -> Double -> Direction3 -> Direction3 -> Wavelength
  -> Maybe (Direction3, Boolean)
nextDirection 
-}

-- select_diffuse

store_photon :: Surface -> Bool
store_photon (Simple _ _ diff _ _ _) = diff > 0.0
store_photon (TS _ _ scat metal _ _ _) = metal /= 0.0 && scat /= 0.0
store_photon _ = True

diffSpec :: Surface -> Color
diffSpec (Simple r _ _ _ _ _) = r
diffSpec (TS ad _ _ _ _ _ _)  = ad
diffSpec _ = black

-- PRIVATE FUNCTIONS

densityPower :: Double -> Double
densityPower rough = 1.0 / (10.0 ** pw + 1.0)
  where
    pw = 5.0 * (1.0 - sqrt rough)

reflectionIndex :: Color -> Double -> Color
reflectionIndex (Color r g b) c =
  Color (r + (1.0 - r) * c2) (g + (1.0 - g) * c2) (b + (1.0 - b) * c2)
  where
    c2 = (1.0 - c) ** 5.0

