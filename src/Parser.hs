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
, rMapDivision
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
, world
, parse
) where

import           Data.Array.Unboxed
import qualified Data.Map.Strict                      as M
import           Data.Maybe
import qualified Data.Vector                          as V
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char   as PC
import qualified Text.ParserCombinators.Parsec.Number as PN
--import           Text.Parsec
--import qualified Text.Parsec.Char   as PC
--import qualified Text.Parsec.Number as PN
import           NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Mapper
import Ray.Material
import Ray.Object
import Ray.Optics
import Ray.Physics
import Ray.Surface

--
-- RESERVED WORD
--

rAlbedoDiff :: String
rAlbedoDiff = "albedo_diff"

rAlbedoSpec :: String
rAlbedoSpec = "albedo_spec"

rAmbient :: String
rAmbient = "ambient"

rAntialias :: String
rAntialias = "antialias"

rCenter :: String
rCenter = "center"

rChecker :: String
rChecker = "checker"

rColor :: String
rColor = "color"

rDiffuseness :: String
rDiffuseness = "diffuseness"

rDir1 :: String
rDir1 = "dir1"

rDir2 :: String
rDir2 = "dir2"

rDirection :: String
rDirection = "direction"

rDirectivity :: String
rDirectivity = "directivity"

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

rLightSpec :: String
rLightSpec = "lightspec"

rMap :: String
rMap = "map"

rMap1 :: String
rMap1 = "map1"

rMap2 :: String
rMap2 = "map2"

rMapDivision :: String
rMapDivision = "mapdivision"

rMapper :: String
rMapper = "mapper"

rMaterial :: String
rMaterial = "material"

rMaxRadiance :: String
rMaxRadiance = "maxradiance"

rMesh :: String
rMesh = "mesh"

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

rRadEst :: String
rRadEst = "rad_est"

rRadiosity :: String
rRadiosity = "radiosity"

rRadius :: String
rRadius = "radius"

rReflectance :: String
rReflectance = "reflectance"

rRoughness :: String
rRoughness = "roughness"

rSamplePhoton :: String
rSamplePhoton = "samplephoton"

rScale :: String
rScale = "scale"

rScatterness :: String
rScatterness = "scatterness"

rSmoothness :: String
rSmoothness = "smoothness"

rSolid :: String
rSolid = "solid"

rSpecularRefl :: String
rSpecularRefl = "specularrefl"

rSphere :: String
rSphere = "sphere"

rSun :: String
rSun = "sun"

rSurface :: String
rSurface = "surface"

rTargetPosition :: String
rTargetPosition = "targetposition"

rTemperature :: String
rTemperature = "temperature"

rTransmittance :: String
rTransmittance = "transmittance"

rType :: String
rType = "type"

rUpperDirection :: String
rUpperDirection = "upperdirection"

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
-- SCREEN SETTINGS
--

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
expecting "nphoton", "progressive", "xresolution", "yresolution", "antialias", "samplephoton", "mapdivision", "estimateradius", "ambient", "maxradiance", "eyeposition", "targetposition", "upperdirection", "focus", "photonfilter", space or end of input
-}

sline :: Parser Param
sline = do
  p <- (try nphoton)     <|>
       (try progressive) <|>
       (try xreso)       <|>
       (try yreso)       <|>
       (try antialias)   <|>
       (try samphoton)   <|>
       (try mapdivision) <|>
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

mapdivision :: Parser Param
mapdivision = do
  _ <- string rMapDivision
  _ <- separator
  i <- integer
  _ <- blanc
  return (rMapDivision, show i)

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

---------------------------------------------

--
-- SCENE INFO : WORLD
--

{-
lightspec:
  ceiling_light:
    temperature: 6500
    radiosity  : 1950
    directivity: 0.0       
    rad_est    : formula   
    direction  : out

  pana_led_ldg11dg95w:
    temperature: 6500      
    radiosity  : 48320     
    directivity: 0.0
    rad_est    : photonmap 
    direction  : out       
-}

sampleworld :: String
sampleworld = concat $ map (\x -> x ++ "\n") [
    "   "
  , "lightspec:"
  , "  ceiling_light:"
  , "    temperature: 6500"
  , "    radiosity: 1950"
  , "    directivity: 0.0"
  , "    rad_est: formula"
  , "    direction: out"
  , "   "
  , "  pana_led_ldg11dg95w:"
  , "    temperature: 3500"
  , "    radiosity: 48320"
  , "    directivity: 0.0"
  , "    rad_est: photon"
  , "    direction: in"    
  , "   "
  , "material:"
  , "  silver:"
  , "    albedo_diff: [ 0.0, 0.0, 0.0 ]"
  , "    scatterness: 0.0"
  , "    metalness: 1.0"
  , "    transmittance: [ 0.0, 0.0, 0.0 ]"
  , "    ior: [ 0.142, 0.128, 0.159 ]"
  , "    albedo_spec: [ 0.974, 0.960, 0.906 ]"
  , "    "
  , "surface:"
  , "  rough:"
  , "    roughness: 0.0"
  , "    light: ceiling_light"
  , "    "
  , "mapper:"
  , "  silverrough:"
  , "    type: solid"
  , "    map: [ silver, rough ]"
  , "  "
  , "object:"
  , "  silverball:"
  , "    type: sphere"
  , "    center: [ 0.0, 5.0, 0.0 ]"
  , "    radius: 0.8"
  , "    mapper: silverrough"
  , "   "
  , "  floor:"
  , "    type: mesh"
  , "    vertex:"
  , "      - [ 0.0 , 0.0, 0.0 ]"
  , "      - [ 1.0, 0.0 , 0.0 ]"
  , "      - [ 0.0, 0.0, 1.0 ]"
  , "    normal:"
  , "      - [ 0.0, 1.0, 0.0 ]"
  , "    uvmap:"
  , "      - [ 0.0, 0.0 ]"
  , "      - [ 1.0, 1.0 ]"
  , "    polygon:"
  , "      - [ [ 0, 0, 0 ], [ 1, 0, 0 ], [ 2, 0, 0 ] ]"
  , "    mapper: silverrough"
  , "   "
  , "scene:"
  ]

{- |
>>> let sample1 = sampleworld ++ "  - silverball\n    \n"
>>> parse world pname sample1
Right [Object {shape = Sphere {center = Vector3 0.0 5.0 0.0, radius = 0.8}, mapper = Solid (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Just (LightSpec {lcolor = [0.3359011141561401,0.33472886389070683,0.329370021953153], radiosity = 2.8550512445095166, directivity = 0.0, radest = Formula, dirflag = Out, cospower = 1.0, power = 0.5, emittance0 = Radiance 0.15263196087942635 0.1520992956124355 0.14966426185249523}), roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0})}]
>>> let sample2 = sampleworld ++ "  - silverplain\n    \n"
>>> parse world pname sample2
Left "rt parser" (line 61, column 1):
unexpected no such an object in the scene: silverplain
expecting lf new-line or "\r\n"
-}

world :: Parser [Object]
world = do
  _  <- many (try linefeed)
  ls <- lightspec_list
  let lgtmap = M.fromList ls
  _  <- many (try linefeed)
  ms <- material_list
  let matemap = M.fromList ms
  _  <- many (try linefeed)
  ss <- surface_list lgtmap
  let sfmap = M.fromList ss
  _  <- many (try linefeed)
  mps <- mapper_list matemap sfmap
  let mapmap = M.fromList mps
  _  <- many (try linefeed)
  os <- object_list mapmap
  let objmap = M.fromList os
  _  <- many (try linefeed)
  sn <- scene
  _  <- many (try linefeed)
--  let
--    mmap = M.fromList ms
--    vmap = M.fromList vs
  scn <- V.forM (V.fromList sn) $ \o ->
    case M.lookup o objmap of
      Nothing  -> unexpected ("no such an object in the scene: " ++ o)
      Just obj -> return obj
  return $ V.toList scn

--
-- LIGHTSPEC LIST
--

{- |
>>> parse lightspec_list pname "        \nlightspec:\n   \n  ceiling:\n    color: [ 1.0, 0.7, 0.4 ]\n    radiosity: 1950\n    directivity: 0.0\n    rad_est: formula\n    direction: out\n   \n  bulb:\n    temperature: 3500\n    radiosity: 48320\n    directivity: 0.0\n    rad_est: photon\n    direction: in\n"
Right [("ceiling",LightSpec {lcolor = [0.47619047619047616,0.3333333333333333,0.19047619047619047], radiosity = 2.8550512445095166, directivity = 0.0, radest = Formula, dirflag = Out, cospower = 1.0, power = 0.5, emittance0 = Radiance 0.21637881825921765 0.15146517278145236 8.655152730368706e-2}),("bulb",LightSpec {lcolor = [0.4334078665789044,0.32723782297452636,0.23935431044656913], radiosity = 70.74670571010249, directivity = 0.0, radest = PhotonMap, dirflag = In, cospower = 1.0, power = 0.5, emittance0 = Radiance 4.880037320284739 3.6845957627160155 2.6950548382296726})]
-}

lightspec_list :: Parser [(String, LightSpec)]
lightspec_list = do
  _ <- many (try space)
  _ <- string rLightSpec
  _ <- separator2
  _ <- linefeed
  ls <- many1 (try lightspec_elem)
  return ls

{- |
>>> parse lightspec_elem pname "   \n  ceiling:\n    color: [ 1.0, 0.7, 0.4 ]\n    radiosity: 1950\n    directivity: 0.0\n    rad_est: formula\n    direction: out\n"
Right ("ceiling",LightSpec {lcolor = [0.47619047619047616,0.3333333333333333,0.19047619047619047], radiosity = 2.8550512445095166, directivity = 0.0, radest = Formula, dirflag = Out, cospower = 1.0, power = 0.5, emittance0 = Radiance 0.21637881825921765 0.15146517278145236 8.655152730368706e-2})
>>> parse lightspec_elem pname "   \n  bulb:\n    temperature: 3500\n    radiosity: 48320\n    directivity: 0.0\n    rad_est: photon\n    direction: in\n"
Right ("bulb",LightSpec {lcolor = [0.4334078665789044,0.32723782297452636,0.23935431044656913], radiosity = 70.74670571010249, directivity = 0.0, radest = PhotonMap, dirflag = In, cospower = 1.0, power = 0.5, emittance0 = Radiance 4.880037320284739 3.6845957627160155 2.6950548382296726})
-}

lightspec_elem :: Parser (String, LightSpec)
lightspec_elem = do
  _ <- many (try linefeed)
  n <- elem_name
  c <- try lcolorP <|> temperature
  r <- radiosityP
  dv <- directivityP
  re <- rad_est
  df <- dir_flag
  return (n, initLightSpec c r dv re df)

{- |
>>> parse lcolorP pname "   \n    color: [ 0.1, 0.9, 0.8 ]\n"
Right [5.555555555555556e-2,0.5,0.4444444444444445]
>>> parse lcolorP pname "   \n    color: [ 1.1, 0.9, 0.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected red of color is out of range: 1.1
>>> parse lcolorP pname "   \n    color: [ 0.1, -0.9, 0.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected green of color is out of range: -0.9
>>> parse lcolorP pname "   \n    color: [ 0.1, 0.9, 2.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected blue of color is out of range: 2.8

>>> parse rad_est pname "   \n    rad_est: photon\n"
Right PhotonMap
>>> parse rad_est pname "   \n    rad_est: formula\n"
Right Formula
>>> parse rad_est pname "   \n    rad_est: abc\n"
Left "rt parser" (line 3, column 1):
unexpected rad_est is 'photon' or 'formula'

>>> parse dir_flag pname "       \n    direction: in\n"
Right In
>>> parse dir_flag pname "       \n    direction: out\n"
Right Out
>>> parse dir_flag pname "       \n    direction: abc\n"
Left "rt parser" (line 2, column 16):
unexpected "a"
expecting "in" or "out"
>>> parse directivityP pname "       \n    directivity: 0.0\n"
Right 0.0
>>> parse directivityP pname "       \n    directivity: 1.0\n"
Right 1.0
>>> parse directivityP pname "       \n    directivity: -1.0\n"
Right (-1.0)
>>> parse directivityP pname "       \n    directivity: 1.1\n"
Left "rt parser" (line 3, column 1):
unexpected directivity (-1.0 - 1.0) is out of range: 1.1
>>> parse directivityP pname "       \n    directivity: -1.1\n"
Left "rt parser" (line 3, column 1):
unexpected directivity (-1.0 - 1.0) is out of range: -1.1
>>> parse radiosityP pname "    \n    radiosity: 0\n"
Right 0.0
>>> parse radiosityP pname "    \n    radiosity: 1000.0\n"
Right 1000.0
>>> parse radiosityP pname "    \n    radiosity: -1000\n"
Left "rt parser" (line 3, column 1):
unexpected radiosity (>= 0.0) is out of range: -1000.0

>>> parse temperature pname "    \n    temperature: 1000\n"
Right [0.789668243169042,0.2103317568309579,0.0]
>>> parse temperature pname "    \n    temperature: 6500.0\n"
Right [0.3359011141561401,0.33472886389070683,0.329370021953153]
>>> parse temperature pname "    \n    temperature: 999\n"
Left "rt parser" (line 3, column 1):
unexpected temperature (1,000 K - 40,000 K) is out of range: 999.0
>>> parse temperature pname "    \n    temperature: 40001\n"
Left "rt parser" (line 3, column 1):
unexpected temperature (1,000 K - 40,000 K) is out of range: 40001.0
-}

lcolorP :: Parser Color
lcolorP = do
  (Color r g b) <- colorparam rColor
  r' <- checkRatio r ("red of color is out of range: " ++ show r)
  g' <- checkRatio g ("green of color is out of range: " ++ show g)
  b' <- checkRatio b ("blue of color is out of range: " ++ show b)
  return $ initColor r' g' b'

temperature :: Parser Color
temperature = do
  s <- floatparam rTemperature
  if s < 1000.0 || s > 40000.0
    then unexpected ("temperature (1,000 K - 40,000 K) is out of range: " ++ show s)
    else return (initColorByKelvin s)

radiosityP :: Parser Double
radiosityP = do
  s <- floatparam rRadiosity
  if s < 0.0
    then unexpected ("radiosity (>= 0.0) is out of range: " ++ show s)
    else return s

directivityP :: Parser Double
directivityP = do
  s <- floatparam rDirectivity
  if s < (-1.0) || s > 1.0
    then unexpected ("directivity (-1.0 - 1.0) is out of range: " ++ show s)
    else return s

rad_est :: Parser RadEstimation
rad_est = do
  re <- nameparam rRadEst
  if re == "photon"
    then return PhotonMap
    else if re == "formula"
      then return Formula
      else unexpected "rad_est is 'photon' or 'formula'"

dir_flag :: Parser InOut
dir_flag = do
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string rDirection
  _ <- separator
  io <- inout
  _ <- linefeed
  return io

--
-- MATERIAL LIST
--

{- |
>>> parse material_list pname "      \nmaterial:\n     \n  glass:\n    albedo_diff: [ 1.0, 1.0, 1.0 ]\n    scatterness: 0.0\n    metalness: 0.0\n    transmittance: [ 1.0, 1.0, 1.0 ]\n    ior: [ 1.467, 1.460, 1.455 ]\n    albedo_spec:\n     \n  gold:\n    albedo_diff: [ 0.0, 0.0, 0.0 ]\n    scatterness: 0.0\n    metalness: 1.0\n    transmittance: [ 0.0, 0.0, 0.0 ]\n    ior: [ 0.161, 0.346, 1.562 ]\n    albedo_spec: [ 0.964, 0.851, 0.392 ]\n"
Right [("glass",Material {albedoDiff = [1.0,1.0,1.0], scatterness = 0.0, metalness = 0.0, transmittance = [1.0,1.0,1.0], ior = [1.467,1.46,1.455], albedoSpec = [3.5834014257760616e-2,3.4965959415691715e-2,3.434945101438937e-2]}),("gold",Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.161,0.346,1.562], albedoSpec = [0.964,0.851,0.392]})]
-}

material_list :: Parser [(String, Material)]
material_list = do
  _ <- many (try space)
  _ <- string rMaterial
  _ <- separator2
  _ <- linefeed
  ms <- many1 (try material_elem)
  return ms

{- |
>>> parse material_elem pname "     \n  glass:\n    albedo_diff: [ 1.0, 1.0, 1.0 ]\n    scatterness: 0.0\n    metalness: 0.0\n    transmittance: [ 1.0, 1.0, 1.0 ]\n    ior: [ 1.467, 1.460, 1.455 ]\n    albedo_spec:\n"
Right ("glass",Material {albedoDiff = [1.0,1.0,1.0], scatterness = 0.0, metalness = 0.0, transmittance = [1.0,1.0,1.0], ior = [1.467,1.46,1.455], albedoSpec = [3.5834014257760616e-2,3.4965959415691715e-2,3.434945101438937e-2]})
>>> parse material_elem pname "     \n  gold:\n    albedo_diff: [ 0.0, 0.0, 0.0 ]\n    scatterness: 0.0\n    metalness: 1.0\n    transmittance: [ 0.0, 0.0, 0.0 ]\n    ior: [ 0.161, 0.346, 1.562 ]\n    albedo_spec: [ 0.964, 0.851, 0.392 ]\n"
Right ("gold",Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.161,0.346,1.562], albedoSpec = [0.964,0.851,0.392]})

-}

material_elem :: Parser (String, Material)
material_elem = do
  n <- elem_name
  ad <- albedo_diff
  s <- scatterness
  m <- metalnessP
  t <- transmittanceP
  i <- iorP
  as <- try albedo_spec <|> noparam rAlbedoSpec Nothing
  return (n, initMaterial ad s m t i as)

{- |
>>> parse albedo_diff pname "   \n    albedo_diff: [ 0.1, 0.9, 0.8 ]\n"
Right [0.1,0.9,0.8]
>>> parse albedo_diff pname "   \n    albedo_diff: [ 1.1, 0.9, 0.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected red of albedo_diff is out of range: 1.1
>>> parse albedo_diff pname "   \n    albedo_diff: [ 0.1, -0.9, 0.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected green of albedo_diff is out of range: -0.9
>>> parse albedo_diff pname "   \n    albedo_diff: [ 0.1, 0.9, 2.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected blue of albedo_diff is out of range: 2.8
>>> parse scatterness pname "   \n    scatterness: 0.8\n"
Right 0.8
>>> parse scatterness pname "   \n    scatterness: 1.8\n"
Left "rt parser" (line 3, column 1):
unexpected scatterness (0.0 - 1.0) is out of range: 1.8
>>> parse scatterness pname "   \n    scatterness: -0.8\n"
Left "rt parser" (line 3, column 1):
unexpected scatterness (0.0 - 1.0) is out of range: -0.8
>>> parse metalnessP pname "   \n    metalness: 0.8\n"
Right 0.8
>>> parse metalnessP pname "   \n    metalness: 1.8\n"
Left "rt parser" (line 3, column 1):
unexpected metalness (0.0 - 1.0) is out of range: 1.8
>>> parse metalnessP pname "   \n    metalness: -0.8\n"
Left "rt parser" (line 3, column 1):
unexpected metalness (0.0 - 1.0) is out of range: -0.8

>>> parse transmittanceP pname "   \n    transmittance: [ 0.1, 0.9, 0.8 ]\n"
Right [0.1,0.9,0.8]
>>> parse transmittanceP pname "   \n    transmittance: [ 1.1, 0.9, 0.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected red of transmittance is out of range: 1.1
>>> parse transmittanceP pname "   \n    transmittance: [ 0.1, -0.9, 0.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected green of transmittance is out of range: -0.9
>>> parse transmittanceP pname "   \n    transmittance: [ 0.1, 0.9, 2.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected blue of transmittance is out of range: 2.8

>>> parse iorP pname "   \n    ior: [ 0.1, 0.9, 0.8 ]\n"
Right [0.1,0.9,0.8]
>>> parse iorP pname "   \n    ior: [ 1.1, 0.9, 0.8 ]\n"
Right [1.1,0.9,0.8]
>>> parse iorP pname "   \n    ior: [ 0.1, -0.9, 0.8 ]\n"
Right [0.1,-0.9,0.8]
>>> parse iorP pname "   \n    ior: [ 0.1, 0.9, 2.8 ]\n"
Right [0.1,0.9,2.8]

>>> parse albedo_spec pname "   \n    albedo_spec: [ 0.1, 0.9, 0.8 ]\n"
Right (Just [0.1,0.9,0.8])
>>> parse albedo_spec pname "   \n    albedo_spec: [ 1.1, 0.9, 0.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected red of albedo_spec is out of range: 1.1
>>> parse albedo_spec pname "   \n    albedo_spec: [ 0.1, -0.9, 0.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected green of albedo_spec is out of range: -0.9
>>> parse albedo_spec pname "   \n    albedo_spec: [ 0.1, 0.9, 2.8 ]\n"
Left "rt parser" (line 3, column 1):
unexpected blue of albedo_spec is out of range: 2.8

-}

albedo_diff :: Parser Color
albedo_diff = do
  (Color r g b) <- colorparam rAlbedoDiff
  r' <- checkRatio r ("red of albedo_diff is out of range: " ++ show r)
  g' <- checkRatio g ("green of albedo_diff is out of range: " ++ show g)
  b' <- checkRatio b ("blue of albedo_diff is out of range: " ++ show b)
  return (Color r' g' b')

scatterness :: Parser Double
scatterness = do
  s <- floatparam rScatterness
  s' <- checkRatio s ("scatterness (0.0 - 1.0) is out of range: " ++ show s)
  return s'

metalnessP :: Parser Double
metalnessP = do
  s <- floatparam rMetalness
  s' <- checkRatio s ("metalness (0.0 - 1.0) is out of range: " ++ show s)
  return s'

transmittanceP :: Parser Color
transmittanceP = do
  (Color r g b) <- colorparam rTransmittance
  r' <- checkRatio r ("red of transmittance is out of range: " ++ show r)
  g' <- checkRatio g ("green of transmittance is out of range: " ++ show g)
  b' <- checkRatio b ("blue of transmittance is out of range: " ++ show b)
  return (Color r' g' b')

iorP :: Parser Color
iorP = do
  c <- colorparam rIor
  return c

albedo_spec :: Parser (Maybe Color)
albedo_spec = do
  (Color r g b) <- colorparam rAlbedoSpec
  r' <- checkRatio r ("red of albedo_spec is out of range: " ++ show r)
  g' <- checkRatio g ("green of albedo_spec is out of range: " ++ show g)
  b' <- checkRatio b ("blue of albedo_spec is out of range: " ++ show b)
  return $ Just (Color r' g' b')

--
-- SURFACE LIST
--

{- |
>>> let l1 = initLightSpec (initColorByKelvin 6500) 1950 0.0 PhotonMap Out
>>> let l2 = initLightSpec (initColorByKelvin 5000) 48320 0.0 Formula Out
>>> let lm = M.fromList [("ceiling", l1), ("bulb", l2)]
>>> parse (surface_list lm) pname "   \nsurface:\n  ceiling:\n    roughness: 0.0\n    light: ceiling\n  floor:\n    roughness: 1.0\n    light:\n  bulb:\n    roughness: 1.0\n    light: bulb\n"
Right [("ceiling",Surface {elight = Just (LightSpec {lcolor = [0.3359011141561401,0.33472886389070683,0.329370021953153], radiosity = 2.8550512445095166, directivity = 0.0, radest = PhotonMap, dirflag = Out, cospower = 1.0, power = 0.5, emittance0 = Radiance 0.15263196087942635 0.1520992956124355 0.14966426185249523}), roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0}),("floor",Surface {elight = Nothing, roughness = 1.0, densityPow = 0.5, alpha = 1.0}),("bulb",Surface {elight = Just (LightSpec {lcolor = [0.3701322600524373,0.3309599290794973,0.29890781086806545], radiosity = 70.74670571010249, directivity = 0.0, radest = Formula, dirflag = Out, cospower = 1.0, power = 0.5, emittance0 = Radiance 4.167573737770148 3.7265055158676947 3.365608667592586}), roughness = 1.0, densityPow = 0.5, alpha = 1.0})]
-}

surface_list :: M.Map String LightSpec -> Parser [(String, Surface)]
surface_list lm = do
  _ <- many (try space)
  _ <- string rSurface
  _ <- separator2
  _ <- linefeed
  ss <- many1 (try (surface_elem lm))
  return ss

{- |
>>> let l1 = initLightSpec (initColorByKelvin 6500) 1950 0.0 PhotonMap Out
>>> let l2 = initLightSpec (initColorByKelvin 5000) 48320 0.0 Formula Out
>>> let lm = M.fromList [("ceiling", l1), ("bulb", l2)]
>>> parse (surface_elem lm) pname "      \n  ceiling:\n    roughness: 0.0\n    light: ceiling\n"
Right ("ceiling",Surface {elight = Just (LightSpec {lcolor = [0.3359011141561401,0.33472886389070683,0.329370021953153], radiosity = 2.8550512445095166, directivity = 0.0, radest = PhotonMap, dirflag = Out, cospower = 1.0, power = 0.5, emittance0 = Radiance 0.15263196087942635 0.1520992956124355 0.14966426185249523}), roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0})
>>> parse (surface_elem lm) pname "      \n  bulb:\n    roughness: 1.0\n    light: bulb\n"
Right ("bulb",Surface {elight = Just (LightSpec {lcolor = [0.3701322600524373,0.3309599290794973,0.29890781086806545], radiosity = 70.74670571010249, directivity = 0.0, radest = Formula, dirflag = Out, cospower = 1.0, power = 0.5, emittance0 = Radiance 4.167573737770148 3.7265055158676947 3.365608667592586}), roughness = 1.0, densityPow = 0.5, alpha = 1.0})
>>> parse (surface_elem lm) pname "      \n  floor:\n    roughness: 1.0\n    light:\n"
Right ("floor",Surface {elight = Nothing, roughness = 1.0, densityPow = 0.5, alpha = 1.0})
-}

surface_elem :: M.Map String LightSpec -> Parser (String, Surface)
surface_elem lm = do
  n <- elem_name
  r <- roughnessP
  l <- try lightparam <|> noparam rLight ""
  lt <- case M.lookup l lm of
    Nothing  -> if l == ""
      then return Nothing
      else unexpected ("lightspec not found (" ++ l ++ ")")
    Just lt' -> return $ Just lt'
  return (n, initSurface lt r)

{- |
>>> parse roughnessP pname "      \n    roughness: 1.0\n"
Right 1.0
>>> parse roughnessP pname "      \n    roughness: 0.5\n"
Right 0.5
>>> parse roughnessP pname "      \n    roughness: 0.0\n"
Right 0.0
>>> parse roughnessP pname "      \n    roughness: 1.5\n"
Left "rt parser" (line 3, column 1):
unexpected roughness(0.0 - 1.0) is out of range: 1.5
>>> parse roughnessP pname "      \n    roughness: -0.5\n"
Left "rt parser" (line 3, column 1):
unexpected roughness(0.0 - 1.0) is out of range: -0.5
-}

roughnessP :: Parser Double
roughnessP = do
  f <- floatparam rRoughness
  f' <- checkRatio f ("roughness(0.0 - 1.0) is out of range: " ++ show f)
  return f'

{- |
>>> parse lightparam pname "      \n    light: pana-100w\n   \n"
Right "pana-100w"
>>> parse lightparam pname "      \n    light:   \n"
Left "rt parser" (line 2, column 14):
unexpected "\n"
expecting "_", uppercase letter, lowercase letter or digit
-}

lightparam :: Parser String
lightparam = do
  s <- nameparam rLight
  return s

--
-- MAPPER LIST
--

{- |
>>> let smat = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.974 0.960 0.906))
>>> let fmat = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
>>> let mm = M.fromList [("silver", smat), ("floor", fmat)]
>>> let psf = initSurface Nothing 0.0
>>> let rsf = initSurface Nothing 1.0
>>> let sm = M.fromList [("polish", psf), ("rough", rsf)]
>>> parse (mapper_list mm sm) pname "     \nmapper:\n  polsilver:\n    type: solid\n    map: [ silver, polish ]\n  chkfloor:\n    type: checker\n    map1: [ silver, polish ]\n    map2: [ floor, rough ]\n    scale: 1.5\n"
Right [("polsilver",Solid (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0})),("chkfloor",Checker (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0}), (Material {albedoDiff = [0.5,0.5,0.5], scatterness = 1.0, metalness = 0.0, transmittance = [0.0,0.0,0.0], ior = [1.534,1.534,1.534], albedoSpec = [4.440882607430812e-2,4.440882607430812e-2,4.440882607430812e-2]},Surface {elight = Nothing, roughness = 1.0, densityPow = 0.5, alpha = 1.0}), 1.5)]
-}

mapper_list :: M.Map String Material -> M.Map String Surface
  -> Parser [(String, Mapper)]
mapper_list mm sm = do
  _ <- many (try space)
  _ <- string rMapper
  _ <- separator2
  _ <- linefeed
  ms <- many1 (try (mapper_elem mm sm))
  return ms

{- |
>>> let smat = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.974 0.960 0.906))
>>> let fmat = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
>>> let mm = M.fromList [("silver", smat), ("floor", fmat)]
>>> let psf = initSurface Nothing 0.0
>>> let rsf = initSurface Nothing 1.0
>>> let sm = M.fromList [("polish", psf), ("rough", rsf)]
>>> parse (mapper_elem mm sm) pname "  polsilver:\n    type: solid\n    map: [ silver, polish ]\n"
Right ("polsilver",Solid (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0}))
>>> parse (mapper_elem mm sm) pname "  chkfloor:\n    type: checker\n    map1: [ silver, polish ]\n    map2: [ floor, rough ]\n    scale: 2\n"
Right ("chkfloor",Checker (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0}), (Material {albedoDiff = [0.5,0.5,0.5], scatterness = 1.0, metalness = 0.0, transmittance = [0.0,0.0,0.0], ior = [1.534,1.534,1.534], albedoSpec = [4.440882607430812e-2,4.440882607430812e-2,4.440882607430812e-2]},Surface {elight = Nothing, roughness = 1.0, densityPow = 0.5, alpha = 1.0}), 2.0)
-}

mapper_elem :: M.Map String Material -> M.Map String Surface
  -> Parser (String, Mapper)
mapper_elem mm sm = do
  n <- elem_name
  m <- mappertype mm sm
  return (n, m)

{- |
>>> let smat = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.974 0.960 0.906))
>>> let fmat = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
>>> let mm = M.fromList [("silver", smat), ("floor", fmat)]
>>> let psf = initSurface Nothing 0.0
>>> let rsf = initSurface Nothing 1.0
>>> let sm = M.fromList [("polish", psf), ("rough", rsf)]
>>> parse (mappertype mm sm) pname "    type: solid\n    map: [ silver, polish ]\n"
Right Solid (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0})
>>> parse (mappertype mm sm) pname "    type: checker\n    map1: [ silver, polish ]\n    map2: [ floor, rough ]\n    scale: 1\n"
Right Checker (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0}), (Material {albedoDiff = [0.5,0.5,0.5], scatterness = 1.0, metalness = 0.0, transmittance = [0.0,0.0,0.0], ior = [1.534,1.534,1.534], albedoSpec = [4.440882607430812e-2,4.440882607430812e-2,4.440882607430812e-2]},Surface {elight = Nothing, roughness = 1.0, densityPow = 0.5, alpha = 1.0}), 1.0
-}

mappertype :: M.Map String Material -> M.Map String Surface
  -> Parser Mapper
mappertype mm sm = do
  s <- try (solid mm sm) <|> try (checker mm sm)
  return s

{- |
>>> let smat = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.974 0.960 0.906))
>>> let fmat = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
>>> let mm = M.fromList [("silver", smat), ("floor", fmat)]
>>> let psf = initSurface Nothing 0.0
>>> let rsf = initSurface Nothing 1.0
>>> let sm = M.fromList [("polish", psf), ("rough", rsf)]
>>> parse (solid mm sm) pname "    type: solid\n    map: [ silver, polish ]\n"
Right Solid (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0})
>>> parse (solid mm sm) pname "    type: solid\n    map: [ gold, polish ]\n"
Left "rt parser" (line 3, column 1):
unexpected material not found (gold)
>>> parse (solid mm sm) pname "    type: solid\n    map: [ silver, glossy ]\n"
Left "rt parser" (line 3, column 1):
unexpected surface not found (glossy)
-}

solid :: M.Map String Material -> M.Map String Surface
  -> Parser Mapper
solid mm sm = do
  _ <- many (try linefeed)
  _ <- ptype rSolid
  _ <- indent2
  _ <- string rMap
  _ <- separator
  (m, s) <- maptuple
  _ <- linefeed
  mt <- case M.lookup m mm of
    Nothing  -> unexpected ("material not found (" ++ m ++ ")")
    Just mt' -> return mt'
  sf <- case M.lookup s sm of
    Nothing  -> unexpected ("surface not found (" ++ s ++ ")")
    Just sf' -> return sf'
  return $ Solid (mt, sf)

{- |
>>> let smat = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.974 0.960 0.906))
>>> let fmat = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
>>> let mm = M.fromList [("silver", smat), ("floor", fmat)]
>>> let psf = initSurface Nothing 0.0
>>> let rsf = initSurface Nothing 1.0
>>> let sm = M.fromList [("polish", psf), ("rough", rsf)]
>>> parse (checker mm sm) pname "    type: checker\n    map1: [ silver, polish ]\n    map2: [ floor, rough ]\n    scale: 0.5\n"
Right Checker (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0}), (Material {albedoDiff = [0.5,0.5,0.5], scatterness = 1.0, metalness = 0.0, transmittance = [0.0,0.0,0.0], ior = [1.534,1.534,1.534], albedoSpec = [4.440882607430812e-2,4.440882607430812e-2,4.440882607430812e-2]},Surface {elight = Nothing, roughness = 1.0, densityPow = 0.5, alpha = 1.0}), 0.5
>>> parse (checker mm sm) pname "    type: checker\n    map1: [ gold, polish ]\n    map2: [ floor, rough ]\n    scale: 0.5\n"
Left "rt parser" (line 5, column 1):
unexpected material not found (gold)
>>> parse (checker mm sm) pname "    type: checker\n    map1: [ silver, glossy ]\n    map2: [ floor, rough ]\n    scale: 0.5\n"
Left "rt parser" (line 5, column 1):
unexpected surface not found (glossy)
>>> parse (checker mm sm) pname "    type: checker\n    map1: [ silver, polish ]\n    map2: [ wall, rough ]\n    scale: 0.5\n"
Left "rt parser" (line 5, column 1):
unexpected material not found (wall)
>>> parse (checker mm sm) pname "    type: checker\n    map1: [ silver, polish ]\n    map2: [ floor, glossy ]\n    scale: 0.5\n"
Left "rt parser" (line 5, column 1):
unexpected surface not found (glossy)
-}

checker :: M.Map String Material -> M.Map String Surface
  -> Parser Mapper
checker mm sm = do
  _ <- many (try linefeed)
  _ <- ptype rChecker
  _ <- indent2
  _ <- string rMap1
  _ <- separator
  (m1, s1) <- maptuple
  _ <- linefeed
  _ <- indent2
  _ <- string rMap2
  _ <- separator
  (m2, s2) <- maptuple
  _ <- linefeed
  sc <- floatparam rScale
  mt1 <- case M.lookup m1 mm of
    Nothing  -> unexpected ("material not found (" ++ m1 ++ ")")
    Just mt1' -> return mt1'
  mt2 <- case M.lookup m2 mm of
    Nothing  -> unexpected ("material not found (" ++ m2 ++ ")")
    Just mt2' -> return mt2'
  sf1 <- case M.lookup s1 sm of
    Nothing  -> unexpected ("surface not found (" ++ s1 ++ ")")
    Just sf1' -> return sf1'
  sf2 <- case M.lookup s2 sm of
    Nothing   -> unexpected ("surface not found (" ++ s2 ++ ")")
    Just sf2' -> return sf2'
  return (Checker (mt1, sf1) (mt2, sf2) sc)

{- |
>>> parse maptuple pname "[ gold, polish ]"
Right ("gold","polish")
-}

maptuple :: Parser (String, String)
maptuple = do
  _ <- char '['
  _ <- many space
  m <- identifier
  _ <- many space
  _ <- char ','
  _ <- many space
  s <- identifier
  _ <- many space
  _ <- char ']'
  return (m, s)

--
-- OBJECT LIST
--

{- |
>>> let smat = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.974 0.960 0.906))
>>> let fmat = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
>>> let psf = initSurface Nothing 0.0
>>> let rsf = initSurface Nothing 1.0
>>> let mm = M.fromList [("silverrough", Solid (smat, rsf)), ("check", Checker (smat, psf) (fmat, psf) 0.5)]
>>> let str = "  \n" ++ "object:\n" ++ "  ball1:\n" ++ "    type: sphere\n" ++ "    center: [ 0.0, 5.0, 3.0 ]\n" ++ "    radius: 0.8\n" ++ "    mapper: silverrough\n" ++ "  floor:\n" ++ "    type: plain\n" ++ "    normal: [ 0.0, 1.0, 0.0 ]\n" ++ "    position: [ 0.0, 0.0, 0.0 ]\n" ++ "    mapper: check\n"
>>> parse (object_list mm) pname str
Right [("ball1",Object {shape = Sphere {center = Vector3 0.0 5.0 3.0, radius = 0.8}, mapper = Solid (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 1.0, densityPow = 0.5, alpha = 1.0})}),("floor",Object {shape = Plain {nvec = Vector3 0.0 1.0 0.0, dist = -0.0}, mapper = Checker (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0}), (Material {albedoDiff = [0.5,0.5,0.5], scatterness = 1.0, metalness = 0.0, transmittance = [0.0,0.0,0.0], ior = [1.534,1.534,1.534], albedoSpec = [4.440882607430812e-2,4.440882607430812e-2,4.440882607430812e-2]},Surface {elight = Nothing, roughness = 0.0, densityPow = 9.99999000001e-7, alpha = 0.0}), 0.5})]

-}

object_list :: M.Map String Mapper -> Parser [(String, Object)]
object_list mm = do
  _ <- many (try linefeed)
  _ <- string "object"
  _ <- separator2
  _ <- linefeed
  o <- many1 (try (object_elem mm))
  return o

{- |
>>> let smat = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.974 0.960 0.906))
>>> let fmat = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
>>> let psf = initSurface Nothing 0.0
>>> let rsf = initSurface Nothing 1.0
>>> let mm = M.fromList [("silverrough", Solid (smat, rsf)), ("check", Checker (smat, psf) (fmat, psf) 0.5)]
>>> parse (object_elem mm) pname "  ball1:\n    type: sphere\n    center: [ 0.0, 5.0, 3.0 ]\n    radius: 0.8\n    mapper: silverrough\n"
Right ("ball1",Object {shape = Sphere {center = Vector3 0.0 5.0 3.0, radius = 0.8}, mapper = Solid (Material {albedoDiff = [0.0,0.0,0.0], scatterness = 0.0, metalness = 1.0, transmittance = [0.0,0.0,0.0], ior = [0.142,0.128,0.159], albedoSpec = [0.974,0.96,0.906]},Surface {elight = Nothing, roughness = 1.0, densityPow = 0.5, alpha = 1.0})})
-}

object_elem :: M.Map String Mapper -> Parser (String, Object)
object_elem mm = do
  n <- elem_name
  s <- oshape
  m <- omapper
  mp <- case M.lookup m mm of
    Nothing  -> unexpected ("mapper not found (" ++ m ++ ")")
    Just mp' -> return mp'
  return (n, initObject s mp)

{- |
>>> parse oshape pname "    type: plain\n    normal: [ 0.0, 1.0, 0.0 ]\n    position: [ 0.0, -1.0, 0.0 ]\n"
Right (Plain {nvec = Vector3 0.0 1.0 0.0, dist = 1.0})
>>> parse sphere pname "    type: sphere\n    center: [ 0.0, 5.0, 3.0 ]\n    radius: 0.8\n"
Right (Sphere {center = Vector3 0.0 5.0 3.0, radius = 0.8})
>>> parse polygon pname "    type: polygon\n    pos1: [ 0.0, 0.0, 0.0 ]\n    pos2: [ 2.0, 0.0, 0.0 ]\n    pos3: [ 0.0, 0.0, 2.0 ]\n    normal:\n"
Right (Polygon {position = Vector3 0.0 0.0 0.0, nvec = Vector3 0.0 (-1.0) 0.0, dir1 = Vector3 2.0 0.0 0.0, dir2 = Vector3 0.0 0.0 2.0})
>>> parse parallelogram pname "    type: parallelogram\n    pos1: [ 0.0, 0.0, 0.0 ]\n    pos2: [ 2.0, 0.0, 0.0 ]\n    pos3: [ 0.0, 0.0, 2.0 ]\n    normal:\n"
Right (Parallelogram {position = Vector3 0.0 0.0 0.0, nvec = Vector3 0.0 (-1.0) 0.0, dir1 = Vector3 2.0 0.0 0.0, dir2 = Vector3 0.0 0.0 2.0})
-}

oshape :: Parser Shape
oshape = do
  s <- try plain         <|>
       try sphere        <|>
       try polygon       <|>
       try parallelogram <|>
       try mesh
  return s

{- |
>>> parse plain pname "    type: plain\n    normal: [ 0.0, 1.0, 0.0 ]\n    position: [ 0.0, -1.0, 0.0 ]\n"
Right (Plain {nvec = Vector3 0.0 1.0 0.0, dist = 1.0})
-}

plain :: Parser Shape
plain = do
  _ <- ptype rPlain
  n <- vector3param rNormal
  p <- vector3param rPosition
  let
    d = -(n <.> p)
  return $ Plain n d

{- |
>>> parse sphere pname "    type: sphere\n    center: [ 0.0, 5.0, 3.0 ]\n    radius: 0.8\n"
Right (Sphere {center = Vector3 0.0 5.0 3.0, radius = 0.8})
-}

sphere :: Parser Shape
sphere = do
  _ <- ptype rSphere
  c <- vector3param rCenter
  r <- floatparam rRadius
  return $ Sphere c r

{- |
>>> parse polygon pname "    type: polygon\n    pos1: [ 0.0, 0.0, 0.0 ]\n    pos2: [ 2.0, 0.0, 0.0 ]\n    pos3: [ 0.0, 0.0, 2.0 ]\n    normal:\n"
Right (Polygon {position = Vector3 0.0 0.0 0.0, nvec = Vector3 0.0 (-1.0) 0.0, dir1 = Vector3 2.0 0.0 0.0, dir2 = Vector3 0.0 0.0 2.0})
>>> parse polygon pname "    type: polygon\n    pos1: [ 0.0, 0.0, 0.0 ]\n    pos2: [ 2.0, 0.0, 0.0 ]\n    pos3: [ 0.0, 0.0, 2.0 ]\n    normal: [ 1.0, 1.0, 0.0 ]\n"
Right (Polygon {position = Vector3 0.0 0.0 0.0, nvec = Vector3 0.7071067811865475 0.7071067811865475 0.0, dir1 = Vector3 2.0 0.0 0.0, dir2 = Vector3 0.0 0.0 2.0})
-}

polygon :: Parser Shape
polygon = do
  _  <- ptype rPolygon
  p1 <- vector3param rPos1
  p2 <- vector3param rPos2
  p3 <- vector3param rPos3
  n  <- try (vector3param rNormal) <|> noparam rNormal o3
  if n == o3
    then return $ initPolygon p1 p2 p3
    else return $ initPolygonWithNormal p1 p2 p3 n

{- |
>>> parse parallelogram pname "    type: parallelogram\n    pos1: [ 0.0, 0.0, 0.0 ]\n    pos2: [ 2.0, 0.0, 0.0 ]\n    pos3: [ 0.0, 0.0, 2.0 ]\n    normal:\n"
Right (Parallelogram {position = Vector3 0.0 0.0 0.0, nvec = Vector3 0.0 (-1.0) 0.0, dir1 = Vector3 2.0 0.0 0.0, dir2 = Vector3 0.0 0.0 2.0})
>>> parse parallelogram pname "    type: parallelogram\n    pos1: [ 0.0, 0.0, 0.0 ]\n    pos2: [ 2.0, 0.0, 0.0 ]\n    pos3: [ 0.0, 0.0, 2.0 ]\n    normal: [ 1.0, 1.0, 0.0 ]\n"
Right (Parallelogram {position = Vector3 0.0 0.0 0.0, nvec = Vector3 0.7071067811865475 0.7071067811865475 0.0, dir1 = Vector3 2.0 0.0 0.0, dir2 = Vector3 0.0 0.0 2.0})
-}

parallelogram :: Parser Shape
parallelogram = do
  _ <- ptype rParallelogram
  p1 <- vector3param rPos1
  p2 <- vector3param rPos2
  p3 <- vector3param rPos3
  n  <- try (vector3param rNormal) <|> noparam rNormal o3
  if n == o3
    then return $ initParallelogram p1 p2 p3
    else return $ initParallelogramWithNormal p1 p2 p3 n

{- |
>>> parse mesh pname "    type: mesh\n    vertex:\n      - [ 0.0, 0.0, 0.0 ]\n      - [ 1.0, 0.0, 0.0 ]\n      - [ 0.0, 0.0, 1.0 ]\n    normal:\n      - [ 0.0, 1.0, 0.0 ]\n    uvmap:\n      - [ 0.0, 0.0 ]\n      - [ 1.0, 1.0 ]\n    polygon:\n      - [ [0, 0, 0], [1, 0, 0], [2, 0, 1] ]\n"
Right (Mesh {patches = [((0,0,0),(1,0,0),(2,0,1))], vertexes = array (0,2) [(0,Vector3 0.0 0.0 0.0),(1,Vector3 1.0 0.0 0.0),(2,Vector3 0.0 0.0 1.0)], normals = array (0,0) [(0,Vector3 0.0 1.0 0.0)], uvmaps = array (0,1) [(0,(0.0,0.0)),(1,(1.0,1.0))]})
-}

mesh :: Parser Shape
mesh = do
  _ <- ptype rMesh
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string "vertex"
  _ <- separator2
  _ <- linefeed
  vs <- many1 (try vector3_elem)
  let vs' = listArray (0, length vs - 1) vs
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string "normal"
  _ <- separator2
  _ <- linefeed
  ns <- many1 (try vector3_elem)
  let ns' = listArray (0, length ns - 1) ns
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string "uvmap"
  _ <- separator2
  _ <- linefeed
  uvs <- many1 (try vector2_elem)
  let uvs' = listArray (0, length uvs - 1) uvs
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string "polygon"
  _ <- separator2
  _ <- linefeed
  ps <- many1 (try polygon_elem)
  return (Mesh (V.fromList ps) vs' ns' uvs')

{- |
>>> parse vector3_elem pname "      - [ 0.0, 1.0, -1.5 ]\n"
Right (Vector3 0.0 1.0 (-1.5))
-}

vector3_elem :: Parser Vector3
vector3_elem = do
  _ <- many (try linefeed)
  _ <- indent
  _ <- indent2
  _ <- string "- "
  v <- vector3
  _ <- linefeed
  return v

vector2_elem :: Parser (Double, Double)
vector2_elem = do
  _ <- many (try linefeed)
  _ <- indent
  _ <- indent2
  _ <- string "- "
  v <- float2
  _ <- linefeed
  return v

{- |
>>> parse polygon_elem pname "      - [ [0, 1, 1], [2, 3, 1], [1, 1, 0]]\n"
Right ((0,1,1),(2,3,1),(1,1,0))
-}

polygon_elem :: Parser (Vertex, Vertex, Vertex)
polygon_elem = do
  _ <- indent
  _ <- indent2
  _ <- char '-'
  _ <- many space
  _ <- char '['
  _ <- many space
  v1 <- vertex_info
  _ <- many space
  _ <- char ','
  _ <- many space
  v2 <- vertex_info
  _ <- many space
  _ <- char ','
  _ <- many space
  v3 <- vertex_info
  _ <- many space
  _ <- char ']'
  _ <- linefeed
  return (v1, v2, v3)

{- |
>>> parse vertex_info pname "[0,1,0]"
Right (0,1,0)
>>> parse vertex_info pname "[ 0 , 1 , 0 ] "
Right (0,1,0)
>>> parse vertex_info pname "[ 0 , a , 0 ] "
Left "rt parser" (line 1, column 7):
unexpected "a"
expecting space or digit
>>> parse vertex_info pname "[ 0 , 1 , -2 ] "
Left "rt parser" (line 1, column 11):
unexpected "-"
expecting space or digit
>>> parse vertex_info pname "[ 1.0 , 1 , 2 ] "
Left "rt parser" (line 1, column 4):
unexpected "."
expecting digit, space or ","
-}

vertex_info :: Parser Vertex
vertex_info = do
  _ <- char '['
  _ <- many space
  i1 <- PN.nat
  _ <- many space
  _ <- char ','
  _ <- many space
  i2 <- PN.nat
  _ <- many space
  _ <- char ','
  _ <- many space
  i3 <- PN.nat
  _ <- many space
  _ <- char ']'
  return (i1, i2, i3)

{- |
>>> parse omapper pname "    mapper: polichgold\n"
Right "polichgold"
>>> parse omapper pname "   mapper: polichgold\n"
Left "rt parser" (line 1, column 1):
unexpected "m"
expecting "    "
>>> parse omapper pname "     mapper: polichgold\n"
Left "rt parser" (line 1, column 5):
unexpected " "
expecting "mapper"
>>> parse omapper pname "    mapper: \n"
Left "rt parser" (line 1, column 13):
unexpected "\n"
expecting "_", uppercase letter, lowercase letter or digit
>>> parse omapper pname "    mappe: polichgold\n"
Left "rt parser" (line 1, column 5):
unexpected ":"
expecting "mapper"
>>> parse omapper pname "    mapper:polichgold\n"
Left "rt parser" (line 1, column 12):
unexpected "p"
-}

omapper :: Parser String
omapper = do
  s <- nameparam rMapper
  return s

--
-- SCENE INFO
--

{- |
>>> parse scene pname ("scene:\n" ++ "  - dragon\n" ++ "  - goblin\n" ++ "  - orc\n")
Right ["dragon","goblin","orc"]
>>> parse scene pname ("  scene:\n" ++ "  - dragon\n" ++ "  - goblin\n" ++ "  - orc\n")
Left "rt parser" (line 1, column 1):
unexpected " "
expecting "scene"
>>> parse scene pname ("scene: " ++ "  - dragon\n" ++ "  - goblin\n" ++ "  - orc\n")
Left "rt parser" (line 1, column 10):
unexpected "-"
expecting lf new-line or "\r\n"
-}

scene :: Parser [String]
scene = do
  _ <- string "scene"
  _ <- separator2
  _ <- linefeed
  objs <- many1 (try scene_elem)
  return objs

{- |
>>> parse scene_elem pname "  - ball1\n"
Right "ball1"
>>> parse scene_elem pname " - ball1\n"
Left "rt parser" (line 1, column 1):
unexpected "-"
expecting "  "
>>> parse scene_elem pname "- ball1\n"
Left "rt parser" (line 1, column 1):
unexpected "-"
expecting lf new-line, "\r\n" or "  "
>>> parse scene_elem pname "  - ball1:\n"
Left "rt parser" (line 1, column 10):
unexpected ":"
expecting "_", "-", uppercase letter, lowercase letter, digit, lf new-line or "\r\n"
>>> parse scene_elem pname "  - ba ll1\n"
Left "rt parser" (line 1, column 8):
unexpected "l"
expecting lf new-line or "\r\n"
>>> parse scene_elem pname "  - ball1 "
Left "rt parser" (line 1, column 11):
unexpected end of input
expecting lf new-line or "\r\n"
>>> parse scene_elem pname "  ba ll1"
Left "rt parser" (line 1, column 3):
unexpected "b"
expecting "- "
>>> parse scene_elem pname "  ball1\n"
Left "rt parser" (line 1, column 3):
unexpected "b"
expecting "- "
-}

scene_elem :: Parser String
scene_elem = do
  _ <- many (try linefeed)
  _ <- indent
  _ <- string "- "
  obj <- identifier
  _ <- linefeed
  return obj

--
-- COMMON PARSER
--

{- |
>>> parse (checkRatio 0.0 "error msg") pname ""
Right 0.0
>>> parse (checkRatio 0.5 "error msg") pname ""
Right 0.5
>>> parse (checkRatio 1.0 "error msg") pname ""
Right 1.0
>>> parse (checkRatio 1.1 "error msg") pname ""
Left "rt parser" (line 1, column 1):
unexpected error msg
>>> parse (checkRatio (-0.1) "error msg") pname ""
Left "rt parser" (line 1, column 1):
unexpected error msg
-}

checkRatio :: Double -> String -> Parser Double
checkRatio r msg = do
  if r > 1.0 || r < 0.0
    then unexpected msg
    else return r

{- |
>>> parse (noparam "name" 0) pname "    name:\n"
Right 0
>>> parse (noparam "name" 0) pname "    name: \n"
Right 0
>>> parse (noparam "name" 0) pname "    name:    \n"
Right 0
>>> parse (noparam "name" "abc") pname "    name:    \n"
Right "abc"
>>> parse (noparam "name" (Just "abc")) pname "    name:    \n"
Right (Just "abc")
>>> parse (noparam "name" Nothing) pname "    name:    \n"
Right Nothing


-}

noparam :: String -> a -> Parser a
noparam pn v = do
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string pn
  _ <- separator2
  _ <- linefeed
  return v

{- |
>>> parse (nameparam "name") pname "    name: abc\n"
Right "abc"
>>> parse (nameparam "name") pname "name: abc\n"
Left "rt parser" (line 1, column 1):
unexpected "n"
expecting lf new-line, "\r\n" or "    "
>>> parse (nameparam "name") pname "    name:abc\n"
Left "rt parser" (line 1, column 10):
unexpected "a"
>>> parse (nameparam "name") pname "    name: abc"
Left "rt parser" (line 1, column 14):
unexpected end of input
expecting "_", "-", uppercase letter, lowercase letter, digit, lf new-line or "\r\n"
-}

nameparam :: String -> Parser String
nameparam pn = do
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string pn
  _ <- separator
  n <- identifier
  _ <- linefeed
  return n

{- |
>>> parse (ptype "plain") pname "    type: plain\n"
Right "plain"
>>> parse (ptype "plain") pname "      \n    type: plain\n"
Right "plain"
-}

ptype :: String -> Parser String
ptype t = do
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string rType
  _ <- separator
  t' <- string t
  _ <- linefeed
  return t'

{- |
>>> parse (colorparam "cparam") pname "    cparam : [ 0.5, 1.0, 0.5 ]\n"
Right [0.5,1.0,0.5]
>>> parse (colorparam "cparam") pname "        \n    cparam : [ 0.5, 1.0, 0.5 ]\n"
Right [0.5,1.0,0.5]
-}

colorparam :: String -> Parser Color
colorparam pn = do
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string pn
  _ <- separator
  c <- color
  _ <- linefeed
  return c

{- |
>>> parse (vector3param "vparam") pname "    vparam : [ 0.5, 1.0, 0.5 ]\n"
Right (Vector3 0.5 1.0 0.5)
>>> parse (vector3param "vparam") pname "        \n    vparam : [ 0.5, 1.0, 0.5 ]\n"
Right (Vector3 0.5 1.0 0.5)
-}

vector3param :: String -> Parser Vector3
vector3param pn = do
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string pn
  _ <- separator
  v <- vector3
  _ <- linefeed
  return v

{- |
>>> parse (floatparam "fparam") pname "    fparam : 0.5 \n"
Right 0.5
>>> parse (floatparam "fparam") pname "        \n    fparam : 0.5 \n"
Right 0.5
-}

floatparam :: String -> Parser Double
floatparam pn = do
  _ <- many (try linefeed)
  _ <- indent2
  _ <- string pn
  _ <- separator
  f <- float
  _ <- linefeed
  return f



{- |
>>> parse elem_name pname "  dragon:\n"
Right "dragon"
>>> parse elem_name pname "        \n  _DRAGON:     \n"
Right "_DRAGON"
>>> parse elem_name pname "  dragon   :\r\n"
Right "dragon"
>>> parse elem_name pname " dragon:\n"
Left "rt parser" (line 1, column 1):
unexpected "d"
expecting "  "
>>> parse elem_name pname "   dragon:\n"
Left "rt parser" (line 1, column 3):
unexpected " "
expecting "_", uppercase letter, lowercase letter or digit
>>> parse elem_name pname "dragon:\n"
Left "rt parser" (line 1, column 1):
unexpected "d"
expecting lf new-line, "\r\n" or "  "
>>> parse elem_name pname "  dragon:orc\n"
Left "rt parser" (line 1, column 10):
unexpected "o"
expecting lf new-line or "\r\n"
>>> parse elem_name pname "  dragon: orc\n"
Left "rt parser" (line 1, column 11):
unexpected "o"
expecting lf new-line or "\r\n"
>>> parse elem_name pname "  dragon:orc \n"
Left "rt parser" (line 1, column 10):
unexpected "o"
expecting lf new-line or "\r\n"
>>> parse elem_name pname "  dragon::\n"
Left "rt parser" (line 1, column 10):
unexpected ":"
expecting lf new-line or "\r\n"
>>> parse elem_name pname "  dra : gon:\n"
Left "rt parser" (line 1, column 9):
unexpected "g"
expecting lf new-line or "\r\n"
-}

elem_name :: Parser String
elem_name = do
  _ <- many (try linefeed)
  _ <- indent
  s <- identifier
  _ <- separator2
  _ <- linefeed
  return s

{- |
>>> parse identifier pname "abc"
Right "abc"
>>> parse identifier pname "_abc"
Right "_abc"
>>> parse identifier pname "-abc"
Left "rt parser" (line 1, column 1):
unexpected "-"
expecting "_", uppercase letter, lowercase letter or digit
>>> parse identifier pname "01abc"
Right "01abc"
>>> parse identifier pname "Abc-"
Right "Abc-"
>>> parse identifier pname "ab-c"
Right "ab-c"
>>> parse identifier pname "a_0bC"
Right "a_0bC"
>>> parse identifier pname "ABC a"
Right "ABC"

-}

identifier :: Parser String
identifier = do
  s1 <- many $ char '_'
  s2 <- idletter <|> digit
  s3 <- many (char '_' <|> char '-' <|> idletter <|> digit)
  return (s1 ++ [s2] ++ s3)

{- |

-}

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
>>> parse indent2 pname "    "
Right ""
>>> parse indent2 pname "     "
Right ""
>>> parse indent2 pname "   "
Left "rt parser" (line 1, column 1):
unexpected end of input
expecting "    "
>>> parse indent2 pname "  1 "
Left "rt parser" (line 1, column 1):
unexpected "1"
expecting "    "
>>> parse indent2 pname "\t  "
Left "rt parser" (line 1, column 1):
unexpected "\t"
expecting "    "
-}

indent2 :: Parser String
indent2 = do
  _ <- string "    "
  return ""

{- |
>>> parse indent pname "  "
Right ""
>>> parse indent pname " "
Left "rt parser" (line 1, column 1):
unexpected end of input
expecting "  "
>>> parse indent pname " \t"
Left "rt parser" (line 1, column 1):
unexpected "\t"
expecting "  "
>>> parse indent pname "a "
Left "rt parser" (line 1, column 1):
unexpected "a"
expecting "  "
-}

indent :: Parser String
indent = do
  _ <- string "  "
  return ""

{- |
>>> parse separator pname ": "
Right ""
>>> parse separator pname " : "
Right ""
>>> parse separator pname "     : "
Right ""
>>> parse separator pname "     :  "
Right ""
>>> parse separator pname "     :\t"
Right ""
>>> parse separator pname " :"          -- YAML grammer error
Left "rt parser" (line 1, column 3):
unexpected end of input
>>> parse separator2 pname " :"          -- YAML grammer error
Right ""
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

separator2 :: Parser String
separator2 = do
  _ <- many space
  _ <- char ':'
  return ""

{- |
>>> parse radiance pname "[ 1.0, -2.0, 3.1e2 ]"
Right (Radiance 1.0 (-2.0) 310.0)
-}

radiance :: Parser Radiance
radiance = do
  (v1, v2, v3) <- float3
  return (Radiance v1 v2 v3)

{- |
>>> parse color pname "[ 1.0, 0.0, -0.5 ]"
Right [1.0,0.0,-0.5]
-}

color :: Parser Color
color = do
  (v1, v2, v3) <- float3
  -- return (initColor v1 v2 v3)
  return (Color v1 v2 v3)

{- |
>>> parse vector3 "rt parser" "[ 1.0, -2.0, 3.1e2 ]"
Right (Vector3 1.0 (-2.0) 310.0)
-}

vector3 :: Parser Vector3
vector3 = do
  (v1, v2, v3) <- float3
  return (Vector3 v1 v2 v3)

float3 :: Parser (Double, Double, Double)
float3 = do
  _ <- string "["
  _ <- many1 space
  d1 <- float
  _ <- many space
  _ <- string ","
  _ <- many1 space
  d2 <- float
  _ <- many space
  _ <- string ","
  _ <- many1 space
  d3 <- float
  _ <- many1 space
  _ <- string "]"
  return (d1, d2, d3)

{- |
>>> parse float2 pname "[ 0.1, 0.2 ]"
Right (0.1,0.2)
>>> parse float2 pname "[ 0.1, -0.2 ]"
Right (0.1,-0.2)
>>> parse float2 pname "[0.1, 0.2 ]"
Left "rt parser" (line 1, column 2):
unexpected "0"
expecting space
>>> parse float2 pname "[ 0.1,0.2 ]"
Left "rt parser" (line 1, column 7):
unexpected "0"
expecting space
>>> parse float2 pname "[ 0.1, 0.2]"
Left "rt parser" (line 1, column 11):
unexpected "]"
expecting digit, exponent or space
-}

float2 :: Parser (Double, Double)
float2 = do
  _ <- string "["
  _ <- many1 space
  d1 <- float
  _ <- string ","
  _ <- many1 space
  d2 <- float
  _ <- many1 space
  _ <- string "]"
  return (d1, d2)

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
>>> parse inout pname "in"
Right In
>>> parse inout pname "out"
Right Out
>>> parse inout pname "yes"
Left "rt parser" (line 1, column 1):
unexpected "y"
expecting "in" or "out"
-}

inout :: Parser InOut
inout = do
  s <- string "in" <|> string "out"
  return $ if s == "in" then In else Out

{- |
>>> parse pfilter pname "none"
Right Nonfilter
>>> parse pfilter pname "cone"
Right Conefilter
>>> parse pfilter pname "gauss"
Right Gaussfilter
>>> parse pfilter pname "cont"
Left "rt parser" (line 1, column 1):
unexpected "t"
expecting "cone"
-}

pfilter :: Parser PhotonFilter
pfilter = do
  s <- string "none" <|> string "cone" <|> string "gauss"
  case s of
    "none"  -> return Nonfilter
    "cone"  -> return Conefilter
    "gauss" -> return Gaussfilter

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
>>> parse idletter pname "a"
Right 'a'
>>> parse idletter pname "A"
Right 'A'
>>> parse idletter pname "_"
Left "rt parser" (line 1, column 1):
unexpected "_"
expecting uppercase letter or lowercase letter
>>> parse idletter pname "-"
Left "rt parser" (line 1, column 1):
unexpected "-"
expecting uppercase letter or lowercase letter
>>> parse idletter pname "5"
Left "rt parser" (line 1, column 1):
unexpected "5"
expecting uppercase letter or lowercase letter
-}

idletter :: Parser Char
idletter = do
  s <- upper <|> lower
  return s

{- |
>>> parse linefeed pname "   \n"
Right ""
>>> parse linefeed pname "\t   \n"
Right ""
>>> parse linefeed pname "\n"
Right ""
>>> parse linefeed pname "\r\n"
Right ""
>>> parse linefeed pname "\r"
Left "rt parser" (line 1, column 1):
unexpected end of input
expecting "\r\n"
>>> parse linefeed pname "bb"
Left "rt parser" (line 1, column 1):
unexpected "b"
expecting lf new-line or "\r\n"
-}

linefeed :: Parser String
linefeed = do
  _ <- many (oneOf " \t")
  _ <- newline <|> crlf
  return ""

{- |
>>> parse crlf pname "a"
Left "rt parser" (line 1, column 1):
unexpected "a"
expecting "\r\n"
>>> parse crlf pname "\r"
Left "rt parser" (line 1, column 1):
unexpected end of input
expecting "\r\n"
>>> parse crlf pname "\n"
Left "rt parser" (line 1, column 1):
unexpected "\n"
expecting "\r\n"
>>> parse crlf pname "\r\n"
Right '\n'
-}

crlf :: Parser Char
crlf = do
  _ <- string "\r\n"
  return '\n'
