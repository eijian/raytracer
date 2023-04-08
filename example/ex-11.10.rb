#!/usr/bin/ruby
#
# ex-11.10
#

require_relative "../util/ppmlib"

def main

  #           12:00 , 15:00 , 17:00 , 17:30     曇天
  suncolor = [  6500,   5000,   3500,   2000,   1000]
  sunradio = [  3500,   3500,   3500,   3000,   1000]
  xangle   = [-54.93, -34.01, -10.72,  -4.69,      0]
  yangle   = [ -3.27,  61.70,  82.30,  86.68,      0]

  skycolor = [ 25000,  18000,   3500,   2500,   6500]
  skyradio = [    40,     30,     15,      5,     80]

  $TIME = 3 # 12:00
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

  ball1 = Sphere.new("ball1", MT[:glass], 0.0, 0.2).move(0, 0.2, 0)
  ball2 = Sphere.new("ball2", MT[:plastic].color(0.5, 0.3, 0.1), 0.0, 0.2).move(0.4, 0.2, -0.3)
  ball3 = Sphere.new("ball3", MT[:copper], 0.0, 0.2).move(-0.4, 0.2, 0.3)
  #ball_glass = Sphere.new("ball_glass", MT[:glass], 0.0, 0.7).move(1, 0.7, 2.6)
  #ball_diamond = Sphere.new("ball_diamond", MT[:diamond], 0.0, 0.7).move(1, 0.7, 2.6)
  #ball_silver = Sphere.new("ball_silver", MT[:silver], 0.0, 0.7).move(-0.9, 0.7, 3.8)

  scene = Scene.new
  scene.put(floor)
  scene.put(sun, sky)
  scene.put(ball1, ball2, ball3)

  puts scene.to_s("outside floor")
end

main

#---
