{-# LANGUAGE NoImplicitPrelude #-}

--
-- Parser
--

module Parser (
  rNPhoton
, rProgressive
, rXresolution
, rYresolution
, rAntialias
, rSamplePhoton
, rUseClassic
, rEstimateRadius
, rAmbient
, rMaxRadiance
, rEyePosition
, rTargetPosition
, rUpperDirection
, rFocus
, rPhotonFilter
, Param
, removeComment
, sline
, scene
, parse
) where

import qualified Data.Map.Strict                      as M
import           Data.Maybe
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char   as PC
import qualified Text.ParserCombinators.Parsec.Number as PN
import           NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Material
import Ray.Object
import Ray.Optics
import Ray.Physics

--
-- RESERVED WORD
--

rAmbient :: String
rAmbient = "ambient"

rAntialias :: String
rAntialias = "antialias"

rCenter :: String
rCenter = "center"

rColor :: String
rColor = "color"

rDiffuseness :: String
rDiffuseness = "diffuseness"

rDir1 :: String
rDir1 = "dir1"

rDir2 :: String
rDir2 = "dir2"

rEmittance :: String
rEmittance = "emittance"

rEstimateRadius :: String
rEstimateRadius = "estimateradius"

rEyePosition :: String
rEyePosition = "eyeposition"

rFlux :: String
rFlux = "flux"

rFocus :: String
rFocus = "focus"

rIor :: String
rIor = "ior"

rLdir :: String
rLdir = "ldir"

rLight :: String
rLight = "light"

rMaterial :: String
rMaterial = "material"

rMaxRadiance :: String
rMaxRadiance = "maxradiance"

rMetalness :: String
rMetalness = "metalness"

rName :: String
rName = "name"

rNormal :: String
rNormal = "normal"

rNPhoton :: String
rNPhoton = "nphoton"

rParallelogram :: String
rParallelogram = "parallelogram"

rPhotonFilter :: String
rPhotonFilter = "photonfilter"

rPlain :: String
rPlain = "plain"

rPoint :: String
rPoint = "point"

rPolygon :: String
rPolygon = "polygon"

rPos1 :: String
rPos1 = "pos1"

rPos2 :: String
rPos2 = "pos2"

rPos3 :: String
rPos3 = "pos3"

rPosition :: String
rPosition = "position"

rPower :: String
rPower = "power"

rProgressive :: String
rProgressive = "progressive"

rRadius :: String
rRadius = "radius"

rReflectance :: String
rReflectance = "reflectance"

rSamplePhoton :: String
rSamplePhoton = "samplephoton"

rSmoothness :: String
rSmoothness = "smoothness"

rSpecularRefl :: String
rSpecularRefl = "specularrefl"

rSphere :: String
rSphere = "sphere"

rSun :: String
rSun = "sun"

rTargetPosition :: String
rTargetPosition = "targetposition"

rTransmittance :: String
rTransmittance = "transmittance"

rType :: String
rType = "type"

rUpperDirection :: String
rUpperDirection = "upperdirection"

rUseClassic :: String
rUseClassic = "useclassic"

rVertex :: String
rVertex = "vertex"

rVertex1 :: String
rVertex1 = "vertex1"

rVertex2 :: String
rVertex2 = "vertex2"

rVertex3 :: String
rVertex3 = "vertex3"


rXresolution :: String
rXresolution = "xresolution"

rYresolution :: String
rYresolution = "yresolution"

--
-- Parsers
--

type Param = (String, String)

pname :: String
pname = "rt parser"

charComment :: Char
charComment = '#'

{- |
>>> removeComment "abc"
"abc"
>>> removeComment "ab#"
"ab"
>>> removeComment "abc #   "
"abc "
>>> removeComment "#abc"
""
-}

removeComment :: String -> String
removeComment []     = []
removeComment (c:cs)
  | c == charComment = []
  | otherwise        = c:(removeComment cs)

--
-- SCENE
--

scene :: Parser ([Light], [(String, Object)])
scene = do
  _  <- spaces
  ls <- lightpart
  _  <- spaces
  ms <- materialpart
  _  <- spaces
  vs <- vertexpart
  _  <- spaces
  let
    mmap = M.fromList ms
    vmap = M.fromList vs
  os <- objectpart mmap vmap
  return (ls, os)
--  return (ls, [])

-- LIGHT

{- |
>>> parse lightpart "rt parser" "light: \n  - type: point \n    color : [ 1.0, 1.0, 0.0 ]\n    flux: 1.0 \n    position: [ -1.0, 2.0, 1.0 ]\n"
Right [[[0.5,0.5,0.0],1.0,Vector3 (-1.0) 2.0 1.0]]
-}

lightpart :: Parser [Light]
lightpart = do
  _ <- string "light:"
  _ <- eoline
  ls <- many1 light
  return ls

{- |
>>> parse light "rt parser" "  - type: point \n    color : [ 1.0, 1.0, 0.0 ]\n    flux: 1.0 \n    position: [ -1.0, 2.0, 1.0 ]\n"
Right [[0.5,0.5,0.0],1.0,Vector3 (-1.0) 2.0 1.0]
>>> parse light "rt parser" "  - type: parallelogram \n    color : [ 1.0, 1.0, 0.0 ]\n    flux: 1.0 \n    position: [ -1.0, 2.0, 1.0 ]\n    dir1: [ 1.0, 0.0, 0.0 ]\n    dir2: [ 0.0, 0.0, 1.0 ]\n"
Right [[0.5,0.5,0.0],1.0,Vector3 (-1.0) 2.0 1.0,Vector3 0.0 (-1.0) 0.0,Vector3 1.0 0.0 0.0,Vector3 0.0 0.0 1.0]
>>> parse light "rt parser" ""
Left "rt parser" (line 1, column 1):
unexpected end of input
expecting space
-}

light :: Parser Light
light = do
  l <- (try pointlight)         <|>
       (try parallelogramlight) <|>
       (try sunlight)
  return l

{- |
>>> parse pointlight "rt parser" "  - type: point\n    color: [ 1.0, 1.0, 0.0 ]\n    flux: 5.0\n    position: [ 1.0, 0.5, 9.4 ]\n"
Right [[0.5,0.5,0.0],5.0,Vector3 1.0 0.5 9.4]
-}

pointlight :: Parser Light
pointlight = do
  _ <- many1 space
  _ <- char '-'
  _ <- many1 space
  _ <- string rType
  _ <- separator
  _ <- string rPoint
  _ <- eoline
  c <- colorparam rColor
  f <- doubleparam rFlux
  p <- vector3param rPosition
  return $ PointLight (normalizeColor c) f p

{- |
>>> parse parallelogramlight "rt parser" "  - type: parallelogram\n    color: [ 1.0, 1.0, 0.0 ]\n    flux: 5.0\n    position: [ 1.0, 0.5, 9.4 ]\n    dir1: [ 1.0, 0.0, 0.0 ]\n    dir2: [ 0.0, 0.0, 1.0 ]\n"
Right [[0.5,0.5,0.0],5.0,Vector3 1.0 0.5 9.4,Vector3 0.0 (-1.0) 0.0,Vector3 1.0 0.0 0.0,Vector3 0.0 0.0 1.0]
>>> parse parallelogramlight "rt parser" "  - type: parallelogram\n    color: [ 1.0, 1.0, 0.0 ]\n    flux: 5.0\n    position: [ 1.0, 0.5, 9.4 ]\n    dir1: [ 1.0, 0.0, 0.0 ]\n    dir2: [ 1.0, 0.0, 0.0 ]\n"
*** Exception: Normal vector is zero.
-}

parallelogramlight :: Parser Light
parallelogramlight = do
  _ <- many1 space
  _ <- char '-'
  _ <- many1 space
  _ <- string rType
  _ <- separator
  _ <- string rParallelogram
  _ <- eoline
  c <- colorparam rColor
  f <- doubleparam rFlux
  p <- vector3param rPosition
  --n <- vector3param rNormal
  d1 <- vector3param rDir1
  d2 <- vector3param rDir2
  let n' = normalize $ d1 <*> d2
  if n' == Nothing
    then error "Normal vector is zero."
    else return $ ParallelogramLight (normalizeColor c) f p (fromJust n') d1 d2

{- |
>>> parse sunlight "rt parser" "  - type: sun\n    color: [ 1.0, 1.0, 0.0 ]\n    flux: 5.0\n    position: [ 1.0, 0.5, 9.4 ]\n    dir1: [ 1.0, 0.0, 0.0 ]\n    dir2: [ 0.0, 0.0, 1.0 ]\n    ldir: [ 0.0, -1.0, 0.0 ]\n"
Right [[0.5,0.5,0.0],5.0,Vector3 1.0 0.5 9.4,Vector3 0.0 (-1.0) 0.0,Vector3 1.0 0.0 0.0,Vector3 0.0 0.0 1.0,Vector3 0.0 (-1.0) 0.0]
-}

sunlight :: Parser Light
sunlight = do
  _ <- many1 space
  _ <- char '-'
  _ <- many1 space
  _ <- string rType
  _ <- separator
  _ <- string rSun
  _ <- eoline
  c <- colorparam rColor
  f <- doubleparam rFlux
  p <- vector3param rPosition
  d1 <- vector3param rDir1
  d2 <- vector3param rDir2
  ld <- vector3param rLdir
  let n' = normalize $ d1 <*> d2
  if n' == Nothing
    then error "Normal vector is zero."
    else return $ SunLight (normalizeColor c) f p (fromJust n') d1 d2 ld

-- MATERIAL

{- |
>>> let h = "material: \n"
>>> let m1 = "  - type: solid \n  name: mparal\n  emittance:     [ 0.7958, 0.7958, 0.7958 ]\n  reflectance:   [ 0.0, 0.0, 0.0 ]\n  transmittance: [ 0.0, 0.0, 0.0 ]\n  specularrefl:  [ 0.0, 0.0, 0.0 ]\n  ior:           [ 0.0, 0.0, 0.0 ]\n  diffuseness:   0.0\n  metalness:     0.0\n  smoothness:    0.0\n"
>>> let m2 = "  - type: solid \n  name: mball\n  emittance:     [ 0.0, 0.0, 0.0 ]\n  reflectance:   [ 1.0, 8.0, 3.0 ]\n  transmittance: [ 0.0, 0.0, 0.0 ]\n  specularrefl:  [ 0.0, 0.0, 0.0 ]\n  ior:           [ 0.0, 0.0, 0.0 ]\n  diffuseness:   0.0\n  metalness:     0.0\n  smoothness:    0.0\n"
>>> parse materialpart pname (h ++ " \n" ++ m1 ++ "   \n" ++ m2)
Right [("mparal",Material {emittance = Radiance 0.7958 0.7958 0.7958, reflectance = [0.0,0.0,0.0], transmittance = [0.0,0.0,0.0], specularRefl = [0.0,0.0,0.0], ior = [0.0,0.0,0.0], diffuseness = 0.0, metalness = 0.0, smoothness = 0.0}),("mball",Material {emittance = Radiance 0.0 0.0 0.0, reflectance = [1.0,8.0,3.0], transmittance = [0.0,0.0,0.0], specularRefl = [0.0,0.0,0.0], ior = [0.0,0.0,0.0], diffuseness = 0.0, metalness = 0.0, smoothness = 0.0})]
-}

materialpart :: Parser [(String, Material)]
materialpart = do
  _ <- string "material:"
--  _ <- separator
  _ <- eoline
  ms <- many1 (try material)
  return ms

{- |
>>> let m = "  - type: solid \n  name: mparal\n  emittance:     [ 0.7958, 0.7958, 0.7958 ]\n  reflectance:   [ 0.0, 0.0, 0.0 ]\n  transmittance: [ 0.0, 0.0, 0.0 ]\n  specularrefl:  [ 0.0, 0.0, 0.0 ]\n  ior:           [ 0.0, 0.0, 0.0 ]\n  diffuseness:   0.0\n  metalness:     0.0\n  smoothness:    0.0\n"
>>> parse material pname m
Right ("mparal",Material {emittance = Radiance 0.7958 0.7958 0.7958, reflectance = [0.0,0.0,0.0], transmittance = [0.0,0.0,0.0], specularRefl = [0.0,0.0,0.0], ior = [0.0,0.0,0.0], diffuseness = 0.0, metalness = 0.0, smoothness = 0.0})
-}

material :: Parser (String, Material)
material = do
  --_ <- spaces
  _ <- many1 space
  _ <- char '-'
  t <- mtype
  n <- mname
  em <- pemittance
  rl <- preflectance
  tr <- ptransmittance
  sp <- pspecularRefl
  ir <- pior
  df <- pdiffuseness
  mt <- pmetalness
  sm <- psmoothness
  return (n, Material em rl tr sp ir df mt sm)

{- |
>>> parse mtype pname "  type  : solid\n"
Right "solid"
>>> parse mtype pname "  type  : solid  \n"
Right "solid"
>>> parse mtype pname "type  : solid\n"
Left "rt parser" (line 1, column 1):
unexpected "t"
expecting space
>>> parse mtype pname "  type  : solid  a"
Left "rt parser" (line 1, column 18):
unexpected "a"
expecting lf new-line
>>> parse mtype pname "  type  : sol\n"
Left "rt parser" (line 1, column 11):
unexpected "\n"
expecting "solid"
>>> parse mtype pname "  type: solid\n"
Right "solid"
>>> parse mtype pname "  type  :solid\n"
Left "rt parser" (line 1, column 10):
unexpected "s"
-}

mtype :: Parser String
mtype = do
  _ <- many1 space
  _ <- string "type"
  _ <- separator
  n <- string "solid"
  _ <- eoline
  return n

mname :: Parser String
mname = nameparam "name"

{- |
>>> parse pemittance pname "    emittance: [ 1.0, 0.5, 1.4 ]\n"
Right (Radiance 1.0 0.5 1.4)
>>> parse pemittance pname "    emittance: [ 1.0, 0.5, 1.4 ] "
Left "rt parser" (line 1, column 34):
unexpected end of input
expecting lf new-line
>>> parse pemittance pname "emittance: [1.0, 0.5, 1.4]\n"
Left "rt parser" (line 1, column 1):
unexpected "e"
expecting space
-}

pemittance :: Parser Radiance
pemittance = do
  _ <- many1 space
  _ <- string "emittance"
  _ <- separator
  e <- radiance
  _ <- eoline
  return e

preflectance :: Parser Color
preflectance = colorparam "reflectance"

ptransmittance :: Parser Color
ptransmittance = colorparam "transmittance"

pspecularRefl :: Parser Color
pspecularRefl = colorparam "specularrefl"

pior :: Parser Color
pior = colorparam "ior"

pdiffuseness :: Parser Double
pdiffuseness = doubleparam "diffuseness"

pmetalness :: Parser Double
pmetalness = doubleparam "metalness"

psmoothness :: Parser Double
psmoothness = doubleparam "smoothness"

-- VERTEX

{- |
>>> parse vertexpart pname "vertex:\n  - p11: [ 0.1, 0.2, 0.3 ]\n  - p12: [ -0.4, -0.5, -0.6 ]\n"
Right [("p11",Vector3 0.1 0.2 0.3),("p12",Vector3 (-0.4) (-0.5) (-0.6))]
-}

vertexpart :: Parser [(String, Position3)]
vertexpart = do
  _ <- string "vertex:"
  _ <- eoline
  vs <- many1 (try vertex)
  return vs

{- |
>>> parse vertex pname "  - p01: [ 1.0, 2.0, 3.0 ]\n"
Right ("p01",Vector3 1.0 2.0 3.0)
>>> parse vertex pname "  - p02 : [ 1.0, 2.0, 3.0 ]\n"
Right ("p02",Vector3 1.0 2.0 3.0)
>>> parse vertex pname "  -p03: [ 1.0, 2.0, 3.0 ]\n"
Left "rt parser" (line 1, column 4):
unexpected "p"
expecting space
>>> parse vertex pname "  - p04: [ -1.0, -2.0, -3.0 ]  \n"
Right ("p04",Vector3 (-1.0) (-2.0) (-3.0))
-}

vertex :: Parser (String, Position3)
vertex = do
  _ <- many1 space
  _ <- char '-'
  _ <- many1 space
  n <- name
  _ <- separator
  p <- vector3
  _ <- eoline
  return (n, p)


-- OBJECT

{- |

-}

objectpart :: M.Map String Material -> M.Map String Position3
           -> Parser [(String, Object)]
objectpart mmap vmap = do
  _ <- string "object:"
  _ <- eoline
  os <- many1 (try $ object mmap vmap)
  return os

{- |
-}

object :: M.Map String Material -> M.Map String Position3
       -> Parser (String, Object)
object mmap vmap = do
  o <- (try $ plain  mmap)             <|>
       (try $ sphere mmap)             <|>
       (try $ parallelogram mmap vmap) <|>
       (try $ polygon mmap vmap)
  return o

{- |
>>> let mmap = M.fromList [("mball", Material radiance0 (Color 0.5 0.2 0.2) black black black 1.0 0.0 0.0)]
>>> parse (plain mmap) pname "  - type: plain\n    name: flooring\n    normal: [ 0.0, 1.0, 0.0 ]\n    position: [ 0.0, 1.0, 0.0 ]\n    material: mball\n"
Right ("flooring",Object (Plain {nvec = Vector3 0.0 1.0 0.0, dist = -1.0}) (Material {emittance = Radiance 0.0 0.0 0.0, reflectance = [0.5,0.2,0.2], transmittance = [0.0,0.0,0.0], specularRefl = [0.0,0.0,0.0], ior = [0.0,0.0,0.0], diffuseness = 1.0, metalness = 0.0, smoothness = 0.0}))
>>> parse (plain mmap) pname "  - type: plain\n    name: ceiling\n    normal: [ 0.0, -1.0, 0.0 ]\n    position: [ 0.0, 4.0, 0.0 ]\n    material: mball\n"
Right ("ceiling",Object (Plain {nvec = Vector3 0.0 (-1.0) 0.0, dist = 4.0}) (Material {emittance = Radiance 0.0 0.0 0.0, reflectance = [0.5,0.2,0.2], transmittance = [0.0,0.0,0.0], specularRefl = [0.0,0.0,0.0], ior = [0.0,0.0,0.0], diffuseness = 1.0, metalness = 0.0, smoothness = 0.0}))
-}

plain :: M.Map String Material -> Parser (String, Object)
plain mmap = do
  _ <- many1 space
  _ <- char '-'
  _ <- ptype rPlain
  nm <- mname
  n  <- vector3param rNormal
  p  <- vector3param rPosition
  mt <- nameparam "material"
  let
    d = n <.> p
  return (nm, initObject (Plain n (-d)) (mmap M.! mt))

{- |
>>> let mmap = M.fromList [("mball", Material radiance0 (Color 0.5 0.2 0.2) black black black 1.0 0.0 0.0)]
>>> parse (sphere mmap) pname "  - type: sphere\n    name: flooring\n    center: [ 0.0, 1.0, 0.0 ]\n    radius: 0.8\n    material: mball\n"
Right ("flooring",Object (Sphere {center = Vector3 0.0 1.0 0.0, radius = 0.8}) (Material {emittance = Radiance 0.0 0.0 0.0, reflectance = [0.5,0.2,0.2], transmittance = [0.0,0.0,0.0], specularRefl = [0.0,0.0,0.0], ior = [0.0,0.0,0.0], diffuseness = 1.0, metalness = 0.0, smoothness = 0.0}))
-}

sphere :: M.Map String Material -> Parser (String, Object)
sphere mmap = do
  _ <- many1 space
  _ <- char '-'
  _ <- ptype rSphere
  nm <- mname
  c  <- vector3param rCenter
  r  <- doubleparam rRadius
  mt <- nameparam "material"
  return (nm, initObject (Sphere c r) (mmap M.! mt))

{- |
>>> let mmap = M.fromList [("mball", Material radiance0 (Color 0.5 0.2 0.2) black black black 1.0 0.0 0.0)]
>>> let vmap = M.fromList [("p1", Vector3 (-0.5) 3.99 2.5), ("p2", Vector3 0.5 3.99 2.5), ("p3", Vector3 (-0.5) 3.99 3.5)]
>>> parse (parallelogram mmap vmap) pname "  - type: parallelogram\n    name: cl\n    pos1: p1\n    pos2: p2\n    pos3: p3\n    material: mball\n"
Right ("cl",Object (Parallelogram {position = Vector3 (-0.5) 3.99 2.5, nvec = Vector3 0.0 (-1.0) 0.0, dir1 = Vector3 1.0 0.0 0.0, dir2 = Vector3 0.0 0.0 1.0}) (Material {emittance = Radiance 0.0 0.0 0.0, reflectance = [0.5,0.2,0.2], transmittance = [0.0,0.0,0.0], specularRefl = [0.0,0.0,0.0], ior = [0.0,0.0,0.0], diffuseness = 1.0, metalness = 0.0, smoothness = 0.0}))
-}

parallelogram :: M.Map String Material -> M.Map String Position3
              -> Parser (String, Object)
parallelogram mmap vmap = do
  _ <- many1 space
  _ <- char '-'
  _ <- ptype rParallelogram
  nm <- mname
  p1 <- nameOrPos3 vmap rPos1
  p2 <- nameOrPos3 vmap rPos2
  p3 <- nameOrPos3 vmap rPos3
  mt <- nameparam "material"
  return (nm, initObject (initParallelogram p1 p2 p3) (mmap M.! mt))

{- |
>>> let mmap = M.fromList [("mball", Material radiance0 (Color 0.5 0.2 0.2) black black black 1.0 0.0 0.0)]
>>> let vmap = M.fromList [("p1", Vector3 (-0.5) 3.99 2.5), ("p2", Vector3 0.5 3.99 2.5), ("p3", Vector3 (-0.5) 3.99 3.5), ("p4", Vector3 1.0 2.0 3.0)]
>>> parse (polygon mmap vmap) pname "  - type: polygon\n    name: tetra\n    pos1: p1\n    pos2: p2\n    pos3: p3\n    material: mball\n"
Right ("tetra",Object (Polygon {position = Vector3 (-0.5) 3.99 2.5, nvec = Vector3 0.0 (-1.0) 0.0, dir1 = Vector3 1.0 0.0 0.0, dir2 = Vector3 0.0 0.0 1.0}) (Material {emittance = Radiance 0.0 0.0 0.0, reflectance = [0.5,0.2,0.2], transmittance = [0.0,0.0,0.0], specularRefl = [0.0,0.0,0.0], ior = [0.0,0.0,0.0], diffuseness = 1.0, metalness = 0.0, smoothness = 0.0}))
-}

polygon :: M.Map String Material -> M.Map String Position3
        -> Parser (String, Object)
polygon mmap vmap = do
  _ <- many1 space
  _ <- char '-'
  _ <- ptype rPolygon
  nm <- mname
  p1 <- nameOrPos3 vmap rPos1
  p2 <- nameOrPos3 vmap rPos2
  p3 <- nameOrPos3 vmap rPos3
  mt <- nameparam "material"
  return (nm, initObject (initPolygon p1 p2 p3) (mmap M.! mt))

{- |
>>> let vmap = M.fromList [("p1", Vector3 (-0.5) 3.99 2.5), ("p2", Vector3 0.5 3.99 2.5), ("p3", Vector3 (-0.5) 3.99 3.5)]
>>> parse (nameOrPos3 vmap "dir1") pname "  dir1: p1\n"
Right (Vector3 (-0.5) 3.99 2.5)
>>> parse (nameOrPos3 vmap "dir1") pname "  dir1: [ 1.0, 2.0, 3.0 ]\n"
Right (Vector3 1.0 2.0 3.0)
-}

nameOrPos3 :: M.Map String Position3 -> String -> Parser Vector3
nameOrPos3 vmap pn = do
  _ <- many1 space
  _ <- string pn
  _ <- separator
  p <- (try $ name2pos vmap) <|> (try vector3)
  _ <- eoline
  return p

name2pos :: M.Map String Position3 -> Parser Vector3
name2pos vmap = do
  n <- name
  return $ vmap M.! n

--
-- COMMON PARSER
--

{- |
>>> parse (nameparam "name") pname "    name: abc\n"
Right "abc"
>>> parse (nameparam "name") pname "name: abc\n"
Left "rt parser" (line 1, column 1):
unexpected "n"
expecting space
>>> parse (nameparam "name") pname "    name:abc\n"
Left "rt parser" (line 1, column 10):
unexpected "a"
>>> parse (nameparam "name") pname "    name: abc"
Left "rt parser" (line 1, column 14):
unexpected end of input
expecting letter or digit or lf new-line
-}

nameparam :: String -> Parser String
nameparam pn = do
  _ <- many1 space
  _ <- string pn
  _ <- separator
  n <- name
  _ <- eoline
  return n

{- |
>>> parse (ptype "plain") pname " type: plain\n"
Right "plain"
-}

ptype :: String -> Parser String
ptype t = do
  _ <- many1 space
  _ <- string rType
  _ <- separator
  t' <- string t
  _ <- eoline
  return t'

{- |
>>> parse (colorparam "cparam") pname "  cparam : [ 0.5, 1.0, 0.5 ]\n"
Right [0.5,1.0,0.5]
-}

colorparam :: String -> Parser Color
colorparam pn = do
  _ <- many1 space
  _ <- string pn
  _ <- separator
  c <- color
  _ <- eoline
  return c

{- |
>>> parse (vector3param "vparam") pname "  vparam : [ 0.5, 1.0, 0.5 ]\n"
Right (Vector3 0.5 1.0 0.5)
-}

vector3param :: String -> Parser Vector3
vector3param pn = do
  _ <- many1 space
  _ <- string pn
  _ <- separator
  v <- vector3
  _ <- eoline
  return v

{- |
>>> parse (doubleparam "dparam") pname "  dparam : 0.5 \n"
Right 0.5
-}

doubleparam :: String -> Parser Double
doubleparam pn = do
  _ <- many1 space
  _ <- string pn
  _ <- separator
  f <- float
  _ <- eoline
  return f

{- |
>>> parse sline pname "xresolution : 256"
Right ("xresolution","256")
>>> parse sline pname "yresolution : 256"
Right ("yresolution","256")
>>> parse sline pname "upperdirection : [ 0.01, 0.02, 0.01 ] "
Right ("upperdirection","Vector3 1.0e-2 2.0e-2 1.0e-2")
>>> parse sline pname "ambient : [ 0.01, 0.02, 0.01 ] "
Right ("ambient","Radiance 1.0e-2 2.0e-2 1.0e-2")
>>> parse sline pname "xreso : 256"
Left "rt parser" (line 1, column 1):
unexpected 'x'
expecting "nphoton", "xresolution", "yresolution", "antialias", "samplephoton", "useclassic", "estimateradius", "ambient", "maxradiance", "eyeposition", "targetposition", "upperdirection", "focus", "photonfilter", space or end of input
-}

sline :: Parser Param
sline = do
  p <- (try nphoton)     <|>
       (try progressive) <|>
       (try xreso)       <|>
       (try yreso)       <|>
       (try antialias)   <|>
       (try samphoton)   <|>
       (try useclassic)  <|>
       (try estradius)   <|>
       (try ambient)     <|>
       (try maxrad)      <|>
       (try eyepos)      <|>
       (try targetp)     <|>
       (try upperd)      <|>
       (try focus)       <|>
       (try pfilt)       <|>
       (try blanc)
  return p

{- |
>>> parse nphoton pname "nphoton : 256"
Right ("nphoton","256")
>>> parse nphoton pname "nphoton :256"           -- YAML error
Left "rt parser" (line 1, column 10):
unexpected "2"
-}

nphoton :: Parser Param
nphoton = do
  _ <- string rNPhoton
  _ <- separator
  i <- integer
  _ <- blanc
  return (rNPhoton, show i)

progressive :: Parser Param
progressive = do
  _ <- string rProgressive
  _ <- separator
  b <- yesno
  _ <- blanc
  return (rProgressive, show b)

{- |
>>> parse xreso pname "xresolution : 256"
Right ("xresolution","256")
>>> parse xreso pname "xresolution: 256"
Right ("xresolution","256")
>>> parse xreso pname "xresolution      : 256"
Right ("xresolution","256")
>>> parse xreso pname "xresolution      : 256  "
Right ("xresolution","256")
>>> parse xreso pname "xresolution :256"         -- YAML error
Left "rt parser" (line 1, column 14):
unexpected "2"
-}

xreso :: Parser Param
xreso = do
  _ <- string rXresolution
  _ <- separator
  i <- integer
  _ <- blanc
  return (rXresolution, show i)

{- |
>>> parse yreso pname "yresolution : 256"
Right ("yresolution","256")
>>> parse yreso pname "yresolution: 256"
Right ("yresolution","256")
>>> parse yreso pname "yresolution      : 256"
Right ("yresolution","256")
>>> parse yreso pname "yresolution :256"         -- YAML error
Left "rt parser" (line 1, column 14):
unexpected "2"
>>> parse yreso pname "xresolution: 256"
Left "rt parser" (line 1, column 1):
unexpected "x"
expecting "yresolution"
-}

yreso :: Parser Param
yreso = do
  _ <- string rYresolution
  _ <- separator
  i <- integer
  _ <- blanc
  return (rYresolution, show i)

antialias :: Parser Param
antialias = do
  _ <- string rAntialias
  _ <- separator
  b <- yesno
  _ <- blanc
  return (rAntialias, show b)

samphoton :: Parser Param
samphoton = do
  _ <- string rSamplePhoton
  _ <- separator
  i <- integer
  _ <- blanc
  return (rSamplePhoton, show i)

useclassic :: Parser Param
useclassic = do
  _ <- string rUseClassic
  _ <- separator
  b <- yesno
  _ <- blanc
  return (rUseClassic, show b)

estradius :: Parser Param
estradius = do
  _ <- string rEstimateRadius
  _ <- separator
  f <- float
  _ <- blanc
  return (rEstimateRadius, show f)

{- |
>>> parse ambient pname "ambient       : [ 0.001, 0.001, 0.001 ]"
Right ("ambient","Radiance 1.0e-3 1.0e-3 1.0e-3")
-}

ambient :: Parser Param
ambient = do
  _ <- string rAmbient
  _ <- separator
  r <- radiance
  _ <- blanc
  return (rAmbient, show r)

{- |
>>> parse maxrad pname "maxradiance : 0.1"
Right ("maxradiance","0.1")
-}

maxrad :: Parser Param
maxrad = do
  _ <- string rMaxRadiance
  _ <- separator
  f <- float 
  _ <- blanc
  return (rMaxRadiance, show f)

eyepos :: Parser Param
eyepos = do
  _ <- string rEyePosition
  _ <- separator
  v <- vector3
  _ <- blanc
  return (rEyePosition, show v)

targetp :: Parser Param
targetp = do
  _ <- string rTargetPosition
  _ <- separator
  v <- vector3
  _ <- blanc
  return (rTargetPosition, show v)

upperd :: Parser Param
upperd = do
  _ <- string rUpperDirection
  _ <- separator
  v <- vector3
  _ <- blanc
  return (rUpperDirection, show v)


focus :: Parser Param
focus = do
  _ <- string rFocus
  _ <- separator
  f <- float
  _ <- blanc
  return (rFocus, show f)

pfilt :: Parser Param
pfilt = do
  _ <- string rPhotonFilter
  _ <- separator
  s <- string "none" <|> string "cone" <|> string "gauss"
  _ <- blanc
  let
    f = case s of
        "none"  -> Nonfilter
        "cone"  -> Conefilter
        "gauss" -> Gaussfilter
        _       -> Nonfilter
  return (rPhotonFilter, show f)

{- |
>>> parse blanc pname ""
Right ("","")
>>> parse blanc pname " "
Right ("","")
>>> parse blanc pname "     "
Right ("","")
>>> parse blanc pname "\t"
Right ("","")
>>> parse blanc pname "\n"
Right ("","")
>>> parse blanc pname "abc"
Left "rt parser" (line 1, column 1):
unexpected 'a'
expecting space or end of input
-}

blanc :: Parser Param
blanc = do
  _ <- many space
  _ <- eof
  return ("", "")

{- |
>>> parse separator pname ": "
Right ""
>>> parse separator pname " : "
Right ""
>>> parse separator pname "     : "
Right ""
>>> parse separator pname " :"          -- YAML grammer error
Left "rt parser" (line 1, column 3):
unexpected end of input
>>> parse separator pname ":"           -- YAML grammer error
Left "rt parser" (line 1, column 2):
unexpected end of input
-}

separator :: Parser String
separator = do
  _ <- many space
  _ <- string ":"
  _ <- many1 (oneOf " \t")
  return ""

{- |
>>> parse radiance pname "[ 1.0, -2.0, 3.1e2 ]"
Right (Radiance 1.0 (-2.0) 310.0)
-}

radiance :: Parser Radiance
radiance = do
  (v1, v2, v3) <- double3
  return (Radiance v1 v2 v3)

{- |
>>> parse color pname "[ 1.0, 0.0, -0.5 ]"
Right [1.0,0.0,-0.5]
-}

color :: Parser Color
color = do
  (v1, v2, v3) <- double3
  -- return (initColor v1 v2 v3)
  return (Color v1 v2 v3)

{- |
>>> parse vector3 "rt parser" "[ 1.0, -2.0, 3.1e2 ]"
Right (Vector3 1.0 (-2.0) 310.0)
-}

vector3 :: Parser Vector3
vector3 = do
  (v1, v2, v3) <- double3
  return (Vector3 v1 v2 v3)

double3 :: Parser (Double, Double, Double)
double3 = do
  _ <- string "["
  _ <- many1 space
  d1 <- float
  _ <- string ","
  _ <- many1 space
  d2 <- float
  _ <- string ","
  _ <- many1 space
  d3 <- float
  _ <- many1 space
  _ <- string "]"
  return (d1, d2, d3)

{- |
>>> parse float "rt parser" "1"
Right 1.0
>>> parse float "rt parser" "1.0"
Right 1.0
>>> parse float "rt parser" "-1"
Right (-1.0)
>>> parse float "rt parser" "-1.0"
Right (-1.0)
>>> parse float "rt parser" "2.0e3"
Right 2000.0
>>> parse float "rt parser" "-2e-3"
Right (-2.0e-3)
-}

float :: Parser Double
float = do
  s <- PN.sign
  --f <- PN.floating <|> PN.int
  v <- PN.decimalFloat
  let f = case v of
          Left  i  -> fromIntegral (i::Int)
          Right f' -> f'
  return $ s f

{- |
>>> parse integer "rt parser" "1"
Right 1
>>> parse integer "rt parser" "-1"
Right (-1)
>>> parse integer "rt parser" "10"
Right 10
>>> parse integer "rt parser" "300"
Right 300
>>> parse integer "rt parser" "01"
Right 1
-}

integer :: Parser Int
integer = do
  s <- PN.sign
  i <- PN.int
  return $ s i

{- |
>>> parse yesno pname "yes"
Right True
>>> parse yesno pname "no"
Right False
>>> parse yesno pname "honto"
Left "rt parser" (line 1, column 1):
unexpected "h"
expecting "yes" or "no"
-}

yesno :: Parser Bool
yesno = do
  s <- string "yes" <|> string "no"
  return $ if s == "yes" then True else False

{- |
>>> parse name pname "yes"
Right "yes"
>>> parse name pname "yes\n"
Right "yes"
>>> parse name pname "  yes"
Left "rt parser" (line 1, column 1):
unexpected " "
expecting letter or digit
>>> parse name pname "001a"
Right "001a"
>>> parse name pname "-!%"
Left "rt parser" (line 1, column 1):
unexpected "-"
expecting letter or digit
>>> parse name pname "a_0"
Right "a_0"
>>> parse name pname "_abc"
Right "_abc"
-}

name :: Parser String
name = do
  --s <- many1 (noneOf " \t\v\f\r\n")
  s <- many1 (alphaNum <|> oneOf "_")
  return s

{- |
>>> parse eoline pname "   \n"
Right ""
>>> parse eoline pname "\t   \n"
Right ""
>>> parse eoline pname "\n"
Right ""
>>> parse eoline pname "bb"
Left "rt parser" (line 1, column 1):
unexpected "b"
expecting lf new-line
-}

eoline :: Parser String
eoline = do
  _ <- many (oneOf " \t")
  _ <- newline
  return ""

