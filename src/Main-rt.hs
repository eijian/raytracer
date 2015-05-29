{-# LANGUAGE NoImplicitPrelude #-}

--
-- Ray tracer w/Photon map
--

module Main where

import System.IO
import Control.Monad
import Data.Trees.KdTree
import NumericPrelude

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
xres = 16 :: Int
yres = 16 :: Int

eex = ex3
eey = negate ey3
stepx = 2.0 / 256 :: Double
stepy = 2.0 / 256 :: Double
origin = focus *> eyedir
  + ((-1.0 + 0.5 * stepx) *> eex)
  - (( 1.0 - 0.5 * stepy) *> eey)
scrmap = [(y, x) | y <- [0..(yres - 1)], x <- [0..(xres - 1)]]  

main :: IO ()
main = do
  (power, photonmap) <- readMap
  let image = map (traceRay 0 power photonmap objs) $ map generateRay scrmap
  outputImage image

readMap :: IO (Double, KdTree PhotonInfo)
readMap = do
  np' <- getLine
  pw' <- getLine
  let np = read np' :: Int
  let pw = read pw' :: Double
  pcs <- forM ([1..np]) $ \i -> do
    l <- getLine
    return $ (read l :: PhotonCache)
  let pmap = fromList $ map convertToInfo pcs
  return (pw, pmap)

outputImage :: [Radiance] -> IO ()
outputImage rs = do
  putStrLn "P3"
  putStrLn "## test"
  putStrLn (show xres ++ " " ++ show yres)
  putStrLn "255"
  forM_ rs $ \i -> do
    putOneCell i
    hPutStrLn stderr "done."

putOneCell :: Radiance -> IO ()
putOneCell (Radiance r g b) = do
  putStr $ show $ radianceToRgb r
  putStr " "
  putStr $ show $ radianceToRgb g
  putStr " "
  putStrLn $ show $ radianceToRgb b

