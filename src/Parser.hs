--
-- Parser
--

module Parser (
  rXresolution
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
, parseLines
, removeComment
) where

import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char   as PC
import qualified Text.ParserCombinators.Parsec.Number as PN

import Ray.Algebra
import Ray.Optics

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

parseLines :: [String] -> [Param]
parseLines []     = []
parseLines (l:ls) = p:(parseLines ls)
  where
    p = case (parse line "rt config parse error" l) of
        Left  e  -> error $ (show e ++ "\nLINE: " ++ l)
        Right p' -> p'

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

{- |
>>> parse line pname "xresolution : 256"
Right ("xresolution","256")
>>> parse line pname "yresolution : 256"
Right ("yresolution","256")
>>> parse line pname "upperdirection : [ 0.01, 0.02, 0.01 ] "
Right ("upperdirection","Vector3 1.0e-2 2.0e-2 1.0e-2")
>>> parse line pname "ambient : [ 0.01, 0.02, 0.01 ] "
Right ("ambient","Radiance 1.0e-2 2.0e-2 1.0e-2")
>>> parse line pname "xreso : 256"
Left "rt parser" (line 1, column 1):
unexpected 'x'
expecting "xresolution", "yresolution", "antialias", "samplephoton", "useclassic", "estimateradius", "ambient", "maxradiance", "eyeposition", "targetposition", "upperdirection", "focus", "photonfilter", space or end of input
-}

line :: Parser Param
line = do
  p <- (try xreso)      <|>
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
expecting space
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
expecting space
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
expecting space
>>> parse separator pname ":"           -- YAML grammer error
Left "rt parser" (line 1, column 2):
unexpected end of input
expecting space
-}

separator :: Parser String
separator = do
  _ <- many space
  _ <- string ":"
  _ <- many1 space
  return ""

{- |
>>> parse radiance pname "[ 1.0, -2.0, 3.1e2 ]"
Right (Radiance 1.0 (-2.0) 310.0)
-}

radiance :: Parser Radiance
radiance = do
  _ <- string "["
  _ <- many1 space
  v1 <- float
  _ <- string ","
  _ <- many1 space
  v2 <- float
  _ <- string ","
  _ <- many1 space
  v3 <- float
  _ <- many1 space
  _ <- string "]"
  return (Radiance v1 v2 v3)

{- |
>>> parse vector3 "rt parser" "[ 1.0, -2.0, 3.1e2 ]"
Right (Vector3 1.0 (-2.0) 310.0)
-}

vector3 :: Parser Vector3
vector3 = do
  _ <- string "["
  _ <- many1 space
  v1 <- float
  _ <- string ","
  _ <- many1 space
  v2 <- float
  _ <- string ","
  _ <- many1 space
  v3 <- float
  _ <- many1 space
  _ <- string "]"
  return (Vector3 v1 v2 v3)

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
