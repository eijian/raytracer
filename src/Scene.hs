{-# LANGUAGE NoImplicitPrelude #-}

--
-- Scene
--

module Scene (
  readScene
) where

import           Data.List
import           Data.Array.Unboxed
import           Data.Maybe
--import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import           NumericPrelude

import Ray.Algebra
import Ray.Geometry
import Ray.Light
import Ray.Mapper
import Ray.Material
--import Ray.Optics
import Ray.Object
import Ray.Physics
import Ray.Surface

import Parser

--
-- CONSTANTS
--

m_air :: Material
--m_air = Material radiance0 white white black (Color 1.0 1.0 1.0) 0.0 0.0 0.0
m_air = initMaterial white 0.0 0.0 white (Color 1.0 1.0 1.0) Nothing

--
-- PUBLIC
--

readScene :: String -> IO (Material, V.Vector Light, V.Vector Object)
readScene file = do
  lines <- readConfig file
  (lgts, objs) <- parseConfig ((intercalate "\n" lines) ++ "\n")
  return (m_air, lgts, objs)

--
-- PRIVATE
--

readConfig :: String -> IO [String]
readConfig file = do
  f <- readFile file
  return $ map removeComment $ lines f

parseConfig :: String -> IO (V.Vector Light, V.Vector Object)
parseConfig conf = do
  let
{-
    vtxs = listArray (0, 5)
      [ Vector3 1.0 1.4 2.6
      , Vector3 1.0 0.7 1.9
      , Vector3 1.7 0.7 2.6
      , Vector3 1.0 0.7 3.3
      , Vector3 0.3 0.7 2.6
      , Vector3 1.0 0.0 2.6
      ]

    norms = listArray (0, 0)
      [ ex3
      ]

    sh_octahedron = Mesh (V.fromList
      [ ((0, 0), (1, 0), (2, 0))  
      , ((0, 0), (2, 0), (3, 0))
      , ((0, 0), (3, 0), (4, 0))
      , ((0, 0), (4, 0), (1, 0))
      , ((5, 0), (1, 0), (4, 0))
      , ((5, 0), (4, 0), (3, 0))
      , ((5, 0), (3, 0), (2, 0))
      , ((5, 0), (2, 0), (1, 0))
      ]) vtxs norms
-}

{-
    vtxs_octahedron = listArray (0, 5)
      [ Vector3 1.4499513267805775 1.3362311101832847 2.6
      , Vector3 1.0 0.8 1.9000000000000001
      , Vector3 0.5500486732194225 0.2637688898167154 2.6
      , Vector3 1.0 0.8 3.3
      , Vector3 0.4637688898167154 1.2499513267805775 2.6
      , Vector3 1.5362311101832846 0.3500486732194225 2.6
      ]
    
    norms_octahedron = listArray (0, 8)
      [ Vector3 (0.0) (0.0) (0.0)
      , Vector3 (-0.07116236596167938) 0.8133895649302385 (-0.5773502691896257)
      , Vector3 (-0.8133895649302385) (-0.0711623659616794) (-0.5773502691896257)
      , Vector3 (-0.8133895649302385) (-0.0711623659616794) 0.5773502691896258
      , Vector3 (-0.07116236596167938) 0.8133895649302383 0.5773502691896258
      , Vector3 0.8133895649302385 0.0711623659616794 (-0.5773502691896257)
      , Vector3 0.07116236596167931 (-0.8133895649302385) (-0.5773502691896257)
      , Vector3 0.07116236596167934 (-0.8133895649302383) 0.577350269189626
      , Vector3 0.8133895649302384 0.0711623659616794 0.5773502691896258
      ]
-}

    vtxs_icosahedron = listArray (0, 11)
      [ Vector3 0.21789934754047335 1.756260369022299 2.0172422684389995
      , Vector3 1.1696347222230137 2.2630668564800875 2.6215690656931017
      , Vector3 2.115940497150065 1.4690220297578684 2.5784309343068985
      , Vector3 0.8303652777769864 (-0.06306685648008736) 2.5784309343068985
      , Vector3 (-0.11594049715006483) 0.7309779702421317 2.6215690656931017
      , Vector3 0.6294701494069217 0.604125740826786 3.5993908270190955
      , Vector3 1.4239989816262781 1.5510083310438865 3.5993908270190955
      , Vector3 1.3705298505930783 1.5958742591732142 1.6006091729809044
      , Vector3 0.5760010183737218 0.6489916689561138 1.6006091729809044
      , Vector3 1.7490549266245894 0.47146828733778257 1.9474432750339974
      , Vector3 1.7821006524595266 0.4437396309777012 3.1827577315610007
      , Vector3 0.2509450733754106 1.7285317126622175 3.252556724966003
      ]
    
    norms_icosahedron = listArray (0, 20)
      [ Vector3 (0.0) (0.0) (0.0)
      , Vector3 0.6100138528453308 0.7076128977698087 0.3566049442262533
      , Vector3 0.5909348739569683 0.7236220619182729 (-0.3566049442262532)
      , Vector3 (-0.5909348739569682) (-0.7236220619182729) 0.3566049442262532
      , Vector3 (-0.6100138528453307) (-0.7076128977698088) (-0.3566049442262532)
      , Vector3 (-0.24820054603520508) 0.20826498663614007 0.9460561211097757
      , Vector3 0.2981499257913436 (-0.25017749276704976) 0.9211502829958905
      , Vector3 0.9445425052120812 (-0.32676682232233495) (-0.032602142215644545)
      , Vector3 0.48582058480923324 (-0.8734503189641445) (-0.03260214221564456)
      , Vector3 0.24820054603520533 (-0.2082649866361403) (-0.9460561211097755)
      , Vector3 (-0.29814992579134364) 0.25017749276704987 (-0.9211502829958903)
      , Vector3 (-0.4858205848092331) 0.8734503189641445 0.03260214221564471
      , Vector3 (-0.9445425052120812) 0.32676682232233495 0.03260214221564461
      , Vector3 0.8285553509596623 0.05843676049787646 0.5568493291893463
      , Vector3 (-0.05545773479574963) 0.8002118147111317 0.5971477968208891
      , Vector3 0.08632815199110302 (-0.8261151703940416) 0.5568493291893463
      , Vector3 (-0.7976849337643089) (-0.08434011618078639) 0.5971477968208889
      , Vector3 0.7976849337643089 0.08434011618078618 (-0.5971477968208889)
      , Vector3 (-0.08632815199110302) 0.8261151703940416 (-0.5568493291893462)
      , Vector3 0.05545773479574966 (-0.8002118147111317) (-0.597147796820889)
      , Vector3 (-0.8285553509596624) (-0.05843676049787637) (-0.5568493291893462)
      ]
    
    sh_icosahedron = Mesh (V.fromList
      [ ((1, 1), (2, 1), (6, 1))
      , ((1, 2), (7, 2), (2, 2))
      , ((3, 3), (4, 3), (5, 3))
      , ((4, 4), (3, 4), (8, 4))
      , ((6, 5), (5, 5), (11, 5))
      , ((5, 6), (6, 6), (10, 6))
      , ((9, 7), (10, 7), (2, 7))
      , ((10, 8), (9, 8), (3, 8))
      , ((7, 9), (8, 9), (9, 9))
      , ((8, 10), (7, 10), (0, 10))
      , ((11, 11), (0, 11), (1, 11))
      , ((0, 12), (11, 12), (4, 12))
      , ((6, 13), (2, 13), (10, 13))
      , ((1, 14), (6, 14), (11, 14))
      , ((3, 15), (5, 15), (10, 15))
      , ((5, 16), (4, 16), (11, 16))
      , ((2, 17), (7, 17), (9, 17))
      , ((7, 18), (1, 18), (0, 18))
      , ((3, 19), (9, 19), (8, 19))
      , ((4, 20), (8, 20), (0, 20))
      ]) vtxs_icosahedron norms_icosahedron


    {-
    (ls, os0) = case (parse scene "rt scene file parse error" conf) of
      Left e -> error (show e)
      Right (l', o') -> (l', o')
    (n, os) = unzip os0
    -}
    sh_ceiling_light = initParallelogram (Vector3 (-0.67) 3.99 2.33) 
      (Vector3 0.67 3.99 2.33) (Vector3 (-0.67) 3.99 3.67)
    --sh_ceiling_bulb1 = Sphere (Vector3 0.0 3.75 3.0) 0.2
    sh_ceiling_bulb1 = Sphere (Vector3 (-1.5) 0.2 1.5) 0.15
    lg_ceiling_light = initLight (initColorByKelvin 6500) 3500 0.0 sh_ceiling_light PhotonMap Out
    --lg_ceiling_bulb1 = initLight (initColorByKelvin 2700) 1370 0.0 sh_ceiling_bulb1 True
    --lg_ceiling_bulb1 = initLight (initColorByKelvin 2700) 1370 0.0 sh_octahedron True
    --lg_ceiling_bulb1 = initLight (initColorByKelvin 2700) 1370 0.0 sh_icosahedron True

    sh_sunlight = initParallelogram (Vector3 4.0 100.0 (-4.0)) (Vector3 4.0 100.0 4.0) (Vector3 (-4.0) 100.0 (-4.0)) 
    sh_skylight = Sphere (Vector3 0 0 0) 1000.0
    lg_sunlight = initLight (initColorByKelvin 6500) 4000 1.0 sh_sunlight PhotonMap Out
    lg_skylight = initLight (initColorByKelvin 12000) 4000 0.0 sh_skylight PhotonMap In

    ls = [
      --ParallelogramLight (initColor 1.0 1.0 1.0) 5.0 (Vector3 (-0.67) 3.99 2.33)
      --  (Vector3 0.0 (-1.0) 0.0) (Vector3 1.33 0.0 0.0) (Vector3 0.0 0.0 1.33)
      --ParallelogramLight (initColorByKelvin 6500) 5.0 (Vector3 (-0.67) 3.99 2.33)
      --  (Vector3 0.0 (-1.0) 0.0) (Vector3 1.33 0.0 0.0) (Vector3 0.0 0.0 1.33)
      lg_ceiling_light
      --, lg_ceiling_bulb1
      --  lg_sunlight
      -- lg_skylight
      ]
    
    {-
    ls = [
      --ParallelogramLight (initColor 1.0 1.0 1.0) 5.0 (Vector3 (-0.67) 3.99 2.33)
      --  (Vector3 0.0 (-1.0) 0.0) (Vector3 1.33 0.0 0.0) (Vector3 0.0 0.0 1.33)
      --ParallelogramLight (initColorByKelvin 6500) 5.0 (Vector3 (-0.67) 3.99 2.33)
      --  (Vector3 0.0 (-1.0) 0.0) (Vector3 1.33 0.0 0.0) (Vector3 0.0 0.0 1.33)
      --  lg_sunlight
       lg_skylight
      ]
    -}

    sf_paral   = initSurface (Just lg_ceiling_light) 0.0
    --sf_bulb1   = initSurface (Just lg_ceiling_bulb1) 1.0
    sf_rough    = initSurface Nothing 1.0
    sf_polish   = initSurface Nothing 0.0
    sf_glossy05 = initSurface Nothing 0.5

    sf_metal  = initSurface Nothing 0.5
    sf_metal2 = initSurface Nothing 0.0
    sf_plastic = initSurface Nothing 0.0
    sf_plastic2 = initSurface Nothing 1.0
    --mball = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.0 1.0 1.0)
    --  (initSurfaceSimple (Color 0.5 0.5 0.5) (Color 0.0 0.0 0.0) 0.5 0.0 0.0)
    mwall  = initMaterial (Color 0.5 0.5 0.5) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
    mwallr = initMaterial (Color 0.4 0.1 0.1) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
    mwallb = initMaterial (Color 0.1 0.1 0.4) 1.0 0.0 black (Color 1.534 1.534 1.534) Nothing
    -- 0.79578はflux 5.0 を半球（2πステラジアン）で割った値。<- 間違い
    --mparal = Material (Radiance 0.7958 0.7958 0.7958) (Color 0.0 0.0 0.0) (Color 0.0 0.0 0.0)
    -- emittanceは flux 5.0 / 半球 2π / ライト面積 1.34^2 = 0.4421。
    --   三原色それぞれがこの輝度を持つとした。
    mparal  = initMaterial black 0.0 0.0 black black Nothing
    glass   = initMaterial (Color 1.0 1.0 1.0) 0.0 0.0 (Color 1.0 1.0 1.0) (Color 1.467 1.460 1.455) Nothing
    diamond = initMaterial (Color 1.0 1.0 1.0) 0.0 0.0 (Color 1.0 1.0 1.0) (Color 2.404 2.42364 2.44984) Nothing
    gold    = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.96 0.76 0.39))
    silver  = initMaterial black 0.0 1.0 black (Color 0.142 0.128 0.159) (Just (Color 0.974 0.960 0.906))
    plastic = initMaterial (Color 0.5 0.30 0.1) 1.0 0.0 black (Color 2.0 2.0 2.0) Nothing
    sky     = initMaterial black 0.0 0.0 black black Nothing
{-
    ypla00 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.0)
    ypla01 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.1)
    ypla02 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.2)
    ypla03 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.3)
    ypla04 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.4)
    ypla05 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.5)
    ypla06 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.6)
    ypla07 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.7)
    ypla08 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 0.8)
    ypla10 = Material radiance0 (Color 0.0 0.0 0.0) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 0.5 0.35 0.1) (Color 0.053 0.053 0.053) 1.0 0.0 1.0)
-}
{-
    ypla00 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.7 1.7 1.7)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.08 0.08 0.08) 0.0 0.0 0.0)
    ypla01 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.1)
    ypla02 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.2)
    ypla03 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.3)
    ypla04 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.4)
    ypla05 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.5)
    ypla06 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.6)
    ypla07 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.7)
    ypla08 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 0.8)
    ypla10 = Material radiance0 (Color 1.0 0.6 0.2) (Color 1.6 1.6 1.6)
      (initSurfaceTS (Color 1.0 1.0 1.0) (Color 0.053 0.053 0.053) 0.0 0.0 1.0)
-}

    map_whwall = uniMapper (mwall, sf_rough)
    map_rdwall = uniMapper (mwallr, sf_rough)
    map_blwall = uniMapper (mwallb, sf_rough)
    map_floor  = uniMapper (mwall, sf_rough)
    --map_floor  = checkerMapper (mwall, sf_rough) (gold, sf_polish)
    --map_floor  = checkerMapper (plastic, sf_rough) (mwall, sf_rough)

    map_clearglass = uniMapper (glass, sf_polish)
    map_cleardiamond = uniMapper (diamond, sf_polish)
    map_smokeglass = uniMapper (glass, sf_glossy05)
    map_polishgold = uniMapper (gold, sf_polish)
    map_glossygold = uniMapper (gold, sf_glossy05)
    map_polishsilver = uniMapper (silver, sf_polish)
    map_smoothplastic = uniMapper (plastic, sf_polish)
    map_roughplastic  = uniMapper (plastic, sf_rough)


    floor = Object (Plain (Vector3 0.0 1.0 0.0) 0.0) map_floor
    ceil  = Object (Plain (Vector3 0.0 (-1.0) 0.0) 4.0) map_whwall
    rsidewall = Object (Plain (Vector3 (-1.0) 0.0 0.0) 2.0) map_blwall
    lsidewall = Object (Plain (Vector3 1.0 0.0 0.0) 2.0) map_rdwall
    backwall = Object (Plain (Vector3 0.0 0.0 1.0) 6.0) map_whwall
    frontwall = Object (Plain (Vector3 0.0 0.0 (-1.0)) 5.0) map_whwall

    ball_glass = Object (Sphere (Vector3 1.0 0.7 2.6) 0.7) map_clearglass
    ball_diamond = Object (Sphere (Vector3 1.0 0.7 2.6) 0.7) map_cleardiamond
    ball_gold = Object (Sphere (Vector3 1.0 0.7 2.6) 0.7) map_glossygold
    ball_silver = Object (Sphere (Vector3 (-0.9) 0.7 3.8) 0.7) map_polishsilver
    ball_plastic = Object (Sphere (Vector3 (-0.9) 0.7 3.8) 0.7) map_smoothplastic
    --octahedron = Object sh_octahedron plastic sf_plastic
    --octahedron = Object sh_octahedron plastic sf_bulb1

    one_ball = Object (Sphere (Vector3 0.0 1.01 0.0) 1.0) map_roughplastic
    icosahedron = Object sh_icosahedron map_clearglass
    --icosahedron = Object sh_icosahedron silver sf_silver
{-
    ball_01 = Object (Sphere (Vector3 (-1.6) 1.2 3.8) 0.4) ypla00
    ball_02 = Object (Sphere (Vector3 (-0.8) 1.2 3.8) 0.4) ypla01
    ball_03 = Object (Sphere (Vector3 ( 0.0) 1.2 3.8) 0.4) ypla02
    ball_04 = Object (Sphere (Vector3 ( 0.8) 1.2 3.8) 0.4) ypla03
    ball_05 = Object (Sphere (Vector3 ( 1.6) 1.2 3.8) 0.4) ypla04
    ball_06 = Object (Sphere (Vector3 (-1.6) 0.4 2.8) 0.4) ypla05
    ball_07 = Object (Sphere (Vector3 (-0.8) 0.4 2.8) 0.4) ypla06
    ball_08 = Object (Sphere (Vector3 ( 0.0) 0.4 2.8) 0.4) ypla07
    ball_09 = Object (Sphere (Vector3 ( 0.8) 0.4 2.8) 0.4) ypla08
    ball_10 = Object (Sphere (Vector3 ( 1.6) 0.4 2.8) 0.4) ypla10
-}
    --ball_silver = Object (Sphere (Vector3 (-0.9) 0.7 3.8) 0.7) mirror

    map_paral = uniMapper (mparal, sf_paral)
    map_skyl  = uniMapper (sky, sf_paral)

    ceiling_light = Object sh_ceiling_light map_paral
    --ceiling_bulb1 = Object sh_ceiling_bulb1 mparal sf_bulb1
    --ceiling_bulb1 = Object sh_octahedron mparal sf_bulb1
    --ceiling_bulb1 = Object sh_icosahedron mparal sf_bulb1
    sunlight = Object sh_sunlight map_paral
    skylight = Object sh_skylight map_skyl

    os = [floor, ceil, rsidewall, lsidewall, backwall, frontwall
        , ceiling_light
        --, ceiling_bulb1
        --, ball_glass
        , ball_diamond
        --, ball_gold
        --, octahedron
        --, icosahedron
        , ball_silver
        --, ball_plastic
        ]
--          ball_01, ball_02, ball_03, ball_04, ball_05,
--          ball_06, ball_07, ball_08, ball_09, ball_10]
    
    {-
    os = [floor, one_ball
         --, sunlight
         , skylight
         ]
    -}
      

  return (V.fromList ls, V.fromList os)
