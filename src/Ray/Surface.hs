{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Surface
--

module Ray.Surface (
  Surface (..)
, emittance
, initSurface
, microfacetNormal
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
--import           Data.Maybe
--import           Debug.Trace
import           GHC.Generics
import           NumericPrelude

import Ray.Algebra
import Ray.Light
import Ray.Optics


-- Surface type

{-
  glossyな表面とは法線ベクトルがブレていると捉える。
  ブレ幅は表面の粗さ(roughness)に依る。
    完全に粗い  ：roughness=1.0
    全く粗くない：roughness=0.0
  
  面の法線をN、glossy面でブレた法線をN'とすると
  <N',N'>(NとN'のなす角をθとした時のcosθ)は、乱数ξ[0,1]を用いて
    cosθ = ξ^(1/n+1)
  とする。ここでnは
    n = 10^(5*(1-√roughness))
  と決める。（roughness 0 〜 1の時の増分に結果がスライドするように）
  cosθの計算時に使えるよう保持しておく
  densityPow <- 1/(n+1)
-}

data Surface = Surface
  { elight     :: !(Maybe Light)
  , roughness  :: !Double
    -- calculate values
  , densityPow :: !Double
  , alpha      :: !Double
  }
  deriving (Eq, Show, Generic)

instance NFData Surface where
  rnf = genericRnf

-- PUBLIC FUNCTIONS

initSurface :: Maybe Light -> Double -> Surface
initSurface lgt rough =
  Surface lgt rough (densityPower (1.0 - sqrt rough)) alpha
  where
    alpha = rough * rough * rough * rough

{-
rough :: Surface -> Double
rough (TS _ _ _ _ rough _ _)  = rough
rough _ = 0.0

diffuseness :: Material -> Double
diffuseness (Material _ _ _ s) = case s of
  (Simple _ _ diff _ _ _) -> diff
  (TS _ _ _ _ rough _ _)  -> rough
  _                       -> 0.0
-}

{-
powerGlossy :: Surface -> Double
powerGlossy (Simple _ _ _ _ _ pow) = pow
powerGlossy (TS _ _ _ _ _ pow _)   = pow
powerGlossy _ = 0.0
-}

{- |
microfacetNormal: 微小平面での法線ベクトルを求める
  IN:  nvec  マクロ平面での法線ベクトル
       vvec  光子/視線の交点への入射ベクトル
       surf  表面状態
       retry 算出した法線ベクトルが無効な場合に再算出するかどうか[0-1]
  OUT: 微小平面での法線ベクトル（Maybe）
-}

microfacetNormal :: Direction3 -> Direction3 -> Surface -> Double
  -> IO (Maybe Direction3)
microfacetNormal nvec vvec surf retry
  | roughness surf == 0.0 = return $ Just nvec    -- 完全平滑面ならマクロ法線を返す
  | otherwise             = do
    nvec' <- blurredVector nvec (densityPow surf)
    if nvec' <.> vvec < 0.0   -- 有効な法線ベクトルが得られた
      then return $ Just nvec'
      else do
        r <- russianRouletteBinary retry
        if r == True
          then microfacetNormal nvec vvec surf retry
          else return Nothing

{- |
emittance


-}

emittance :: Surface -> Position3 -> Direction3 -> Radiance
emittance (Surface Nothing _ _ _) _ _ = radiance0
emittance (Surface (Just lgt) _ _ _) pos vvec = lemittance lgt pos vvec


-- PRIVATE FUNCTIONS

{-
reflectionIndex :: Color -> Double -> Color
reflectionIndex (Color r g b) c =
  Color (r + (1.0 - r) * c2) (g + (1.0 - g) * c2) (b + (1.0 - b) * c2)
  where
    c2 = (1.0 - c) ** 5.0
-}
