#!/usr/bin/ruby
#
# ex-11.10
#

require_relative "../util/ppmlib"

CWD = File.expand_path(File.dirname(__FILE__))

def main

  #           12:00 , 15:00 , 17:00 , 17:30     曇天
  suncolor = [  6500,   5000,   3500,   2000,   1000]
  sunradio = [  3500,   3500,   3500,   3000,   1000]
  xangle   = [-54.93, -34.01, -10.72,  -4.69,      0]
  yangle   = [ -3.27,  61.70,  82.30,  86.68,      0]

  skycolor = [ 25000,  18000,   3500,   2500,   6500]
  skyradio = [    40,     30,     15,      5,     80]

  $TIME = 1 # 12:00
  sunlight = LightSpec.new(suncolor[$TIME], 0.5, LightSpec::EST_FORMULA, LightSpec::DIR_OUT, sunradio[$TIME])
  skylight = LightSpec.new(skycolor[$TIME], 0.0, LightSpec::EST_FORMULA, LightSpec::DIR_IN , skyradio[$TIME])

  sun = Mesh.new("sun", MT[:light], 0.0).emit(sunlight, 0)
            .rotate(-90, 0, 0).move(0, 0, 4)
            .rotate(xangle[$TIME], yangle[$TIME], 0)
  sky = Sphere.new("sky", MT[:light], 0.0, 10.0).emit(skylight, 0)

  # objects

  whitecarpet = MT[:carpet].color(0.5, 0.5, 0.5)
  graycarpet  = MT[:carpet].color(0.2, 0.2, 0.2)
  carpetsurf  = Surface.new(1.0)
  checkmap = Checker.new([whitecarpet, graycarpet], [carpetsurf, carpetsurf], 4.5)
  floor = Mesh.new("floor").scale(2.6).mapping(checkmap)

  #octahedron = Mesh::read_file("octahedron", MT[:diamond], 0.0, CWD + "/octahedron.obj", Mesh::FILE_OBJ)
  #                   .scale(0.3).rotate(20, 10, 0).move(0, 0.3, 0)
  bunny = Mesh::read_file("bunny", MT[:plastic].color(0.1, 0.3, 0.5), 0.0, CWD + "/BunnyLowPoly.obj", Mesh::FILE_OBJ)
                     .scale(0.01).rotate(0, 30, 0).move(0, 0, 0)
  #ball1 = Sphere.new("ball1", MT[:glass], 0.0, 0.2).move(0, 0.2, 0)
  ball2 = Sphere.new("ball2", MT[:plastic].color(0.5, 0.3, 0.1), 0.0, 0.2).move(0.4, 0.2, -0.3)
  ball3 = Sphere.new("ball3", MT[:copper], 0.0, 0.2).move(-0.4, 0.2, 0.3)

  scene = Scene.new
  scene.put(floor)
  scene.put(sun, sky)
#  scene.put(octahedron, ball2, ball3)
  scene.put(bunny, ball2, ball3)

  puts scene.to_s("outside floor")
end

main

#---
