{-# LANGUAGE NoImplicitPrelude #-}

--
-- Ray tracer w/Photon map
--

module Main where

import System.IO
import Control.Monad
--import Data.Trees.KdTree
import Data.KdTree.Static
import NumericPrelude
import Debug.Trace

import Ray.Algebra
import Ray.Geometry
import Ray.Object
import Ray.Optics
import Tracer
import Scene

eyepos = initPos 0 2 0
eyedir = ez3
upper = ey3
focus = 1.0 :: Double
xres = 256 :: Int
yres = 256 :: Int

stepx = 2.0 / fromIntegral xres :: Double
stepy = 2.0 / fromIntegral yres :: Double
eex = ex3
eey = negate ey3
{-
origin = focus *> eyedir
  + ((-1.0 + 0.5 * stepx) *> eex)
  - (( 1.0 - 0.5 * stepy) *> eey)
-}
origin = initPos (-1.0) 3.0 1.0
step = (stepx, stepy)
evec = (eex, eey)
generateRay' = generateRay eyepos origin step evec

scrmap = [(y, x) | y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  

main :: IO ()
main = do
  let image = map (traceRay' 0 lgts objs) $ map generateRay' scrmap
  outputImage image

{-
readMap :: IO (Double, KdTree Double PhotonInfo)
readMap = do
  np' <- getLine
  pw' <- getLine
  let np = read np' :: Int
  let pw = read pw' :: Double
  pcs <- forM ([1..np]) $ \i -> do
    l <- getLine
    return $ (read l :: PhotonCache)
  --let pmap = fromList $ map convertToInfo pcs
  let pmap = build infoToPointList (map convertToInfo pcs)
  return (pw, pmap)
-}

outputImage :: [Radiance] -> IO ()
outputImage rs = do
  putStrLn "P3"
  putStrLn "## test"
  putStrLn (show xres ++ " " ++ show yres)
  putStrLn "255"
  forM_ rs $ \i -> do
    putStrLn (showOneCell i)
    hPutStrLn stderr "done."

showOneCell :: Radiance -> String
showOneCell (Radiance r g b) =
  (show $ radianceToRgb clip r) ++ " " ++
  (show $ radianceToRgb clip g) ++ " " ++
  (show $ radianceToRgb clip b)


