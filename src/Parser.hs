--
-- Parser
--

module Parser (
  rNPhoton
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

--import qualified Data.Map.Strict                      as M
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char   as PC
import qualified Text.ParserCombinators.Parsec.Number as PN

import Ray.Algebra
import Ray.Material
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

rEmittance :: String
rEmittance = "emittance"

rEstimateRadius :: String
rEstimateRadius = "estimateradius"

rEyePosition :: String
rEyePosition = "eyeposition"

rFocus :: String
rFocus = "focus"

rIor :: String
rIor = "ior"

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

rPolygon :: String
rPolygon = "polygon"

rPosition :: String
rPosition = "position"

rPower :: String
rPower = "power"

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

scene :: Parser [(String, Material)]
scene = do
  _  <- spaces
  ms <- materialpart
  return (ms)

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
  ms <- many1 material
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
  p <- (try nphoton)    <|>
       (try xreso)      <|>
       (try yreso)      <|>
       (try antialias)  <|>
       (try samphoton)  <|>
       (try useclassic) <|>
       (try estradius)  <|>
       (try ambient)    <|>
       (try maxrad)     <|>
       (try eyepos)     <|>
       (try target)     <|>
       (try upperd)     <|>
       (try focus)      <|>
       (try pfilt)      <|>
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

target :: Parser Param
target = do
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
        "none"    -> Nonfilter
        "cone"    -> Conefilter
        "gauss"   -> Gaussfilter
        othereise -> Nonfilter
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
          Left  i -> fromIntegral i
          Right f -> f
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
-}

name :: Parser String
name = do
  --s <- many1 (noneOf " \t\v\f\r\n")
  s <- many1 alphaNum
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

