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
) where



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




