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

-- for screen configuration

rXresolution :: String
rXresolution = "xresolution"

rYresolution :: String
rYresolution = "yresolution"

rAntialias :: String
rAntialias = "antialias"

rSamplePhoton :: String
rSamplePhoton = "samplephoton"

rUseClassic :: String
rUseClassic = "useclassic"

rEstimateRadius :: String
rEstimateRadius = "estimateradius"

rAmbient :: String
rAmbient = "ambient"

rMaxRadiance :: String
rMaxRadiance = "maxradiance"

rEyePosition :: String
rEyePosition = "eyeposition"

rTargetPosition :: String
rTargetPosition = "targetposition"

rUpperDirection :: String
rUpperDirection = "upperdirection"

rFocus :: String
rFocus = "focus"

rPhotonFilter :: String
rPhotonFilter = "photonfilter"


