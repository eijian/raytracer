{-# LANGUAGE NoImplicitPrelude #-}

--
-- Screen module
--

module Screen where

import System.IO
import Control.Monad
import Data.Maybe
import NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Optics
import Scene

-- PARAMETERS --

-- for camera

eyepos = initPos 0 2 0
eyedir = ez3
upper = ey3
focus = 1.0 :: Double
xres = 256 :: Int
yres = 256 :: Int

stepx = 2.0 / fromIntegral xres :: Double
stepy = 2.0 / fromIntegral yres :: Double
step = (stepx, stepy)
eex = ex3
eey = negate ey3
evec = (eex, eey)
origin = eyepos + focus *> eyedir
  + ((-1.0 + 0.5 * stepx) *> eex)
  - (( 1.0 - 0.5 * stepy) *> eey)
scrmap = [(y, x) | y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  

-- FUNCTIONS --

generateRay :: Position3 -> Position3 -> (Double, Double)
            -> (Direction3, Direction3) -> (Int, Int) -> Ray
generateRay e o (sx, sy) (ex, ey) (y, x) = initRay e edir'
  where
    tgt   = o + ((sx * fromIntegral x) *> ex) + ((sy * fromIntegral y) *> ey)
    edir  = tgt - e 
    edir' = fromJust $ normalize edir

generateRay' = generateRay eyepos origin step evec


outputImage :: [Radiance] -> IO ()
outputImage rs = do
  putStrLn "P3"
  putStrLn "## test"
  putStrLn (show xres ++ " " ++ show yres)
  putStrLn "255"
  forM_ rs $ \i -> do
    putStrLn $ convertOneCell i

convertOneCell :: Radiance -> [Char]
convertOneCell (Radiance r g b) =
  (show $ radianceToRgb clip r) ++ " " ++
  (show $ radianceToRgb clip g) ++ " " ++
  (show $ radianceToRgb clip b)



