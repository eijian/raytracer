{-# LANGUAGE NoImplicitPrelude #-}

--
-- Antialias
--

module Antialias (
  smooth
, smooth2
) where

import qualified Data.Vector as V
import NumericPrelude

import Ray.Geometry
import Ray.Optics
import Ray.Physics

import Screen

--
-- CONSTANTS
--

diffAliasing :: Int
diffAliasing = 20

diffAliasing' :: Double
diffAliasing' = 0.1

--

blur :: [(Double, Double)]
blur = [(-0.25, -0.25), (-0.25,  0.25), ( 0.25, -0.25), ( 0.25,  0.25)]

bmag :: Double
bmag = 1.0 / (fromIntegral $ (length blur + 1))

bmagcolor :: Color
bmagcolor = Color bmag bmag bmag

markGreen :: Radiance
markGreen = Radiance 0.0 1.0 0.0

smooth :: (Ray -> IO Radiance) -> Screen -> V.Vector Rgb -> Int -> IO Rgb
smooth tracer scr ims i
  | antialias scr == False            = return (ims V.! i)
  | isDifferent i ims offset == False = return (ims V.! i)
  | otherwise = do
    l <- retrace tracer scr i
    return $ avg ((ims V.! i):(map (radianceToRgb scr) l))
  where
    xr = xreso scr
    offset = [-xr-1, -xr, -xr+1, -1, 1, xr-1, xr, xr+1]

smooth2 :: (Ray -> IO Radiance) -> Screen -> V.Vector Radiance -> Int
        -> IO Radiance
smooth2 tracer scr ims i = return (ims V.! i)
{-
  | antialias scr == False                                 = return (ims V.! i)
  | isDifferent2 (radianceToRgb scr) i ims offset == False = return (ims V.! i)
  | otherwise = do
    l <- retrace tracer scr i
    return $ avg2 scr ((ims V.! i):l)
  where
    xr = xreso scr
    offset = [-xr-1, -xr, -xr+1, -1, 1, xr-1, xr, xr+1]
-}

avg :: [Rgb] -> Rgb
avg ls = (r `div` len, g `div` len, b `div` len)
  where
    len = length ls
    (r, g, b) = avg' ls

avg' :: [Rgb] -> Rgb
avg' [] = (0, 0, 0)
avg' ((r1, g1, b1):ls) = (r1 + r2, g1 + g2, b1 + b2)
  where
    (r2, g2, b2) = avg' ls

avg2 :: Screen -> [Radiance] -> Radiance
avg2 scr ls = bmagcolor <**> (sum ls)
--avg2 scr ls = rgbToRadiance scr (avg $ map (radianceToRgb scr) ls)
    
{-

  IN : position in the vector
       vector of Radiance
       image offset
  OUT: true = have to trace a detail of the cell
       false = no need to detail tracing
-}

isDifferent :: Int -> V.Vector Rgb -> [Int] ->  Bool
isDifferent _ _ [] = False
isDifferent p rs (i:is)
  | p' <  0         = isDifferent p rs is
  | p' >= length rs = isDifferent p rs is
  | df              = True
  | otherwise       = isDifferent p rs is
  where
    p' = p + i
    df = diffRgb (rs V.! p) (rs V.! p')

isDifferent2 :: (Radiance -> Rgb) -> Int -> V.Vector Radiance -> [Int] -> Bool
isDifferent2 rfunc _ _ [] = False
isDifferent2 rfunc p rs (i:is)
  | p' < 0          = isDifferent2 rfunc p rs is
  | p' >= length rs = isDifferent2 rfunc p rs is
  | df              = True
  | otherwise       = isDifferent2 rfunc p rs is
  where
    p' = p + i
    r1 = rfunc (rs V.! p)
    r2 = rfunc (rs V.! p')
    df = diffRgb r1 r2

diffRgb :: Rgb -> Rgb -> Bool
diffRgb (r1, g1, b1) (r2, g2, b2) =
  abs (r1 - r2) > diffAliasing ||
  abs (g1 - g2) > diffAliasing ||
  abs (b1 - b2) > diffAliasing
  where
    abs :: Int -> Int
    abs i = if i < 0 then (-i) else i
    
retrace :: (Ray -> IO Radiance) -> Screen -> Int -> IO [Radiance]
retrace tracer scr p = do
  let p' = ( fromIntegral (p `div` (xreso scr))
           , fromIntegral (p `mod` (xreso scr)))
  ray <- mapM (generateRay scr) (map (badd p') blur)
  mapM tracer ray

badd :: (Double, Double) -> (Double, Double) -> (Double, Double)
badd (px, py) (bx, by) = (px + bx, py + by)
