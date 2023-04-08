#!/usr/bin/ruby
#
# ex-11.10
#

require_relative "../util/ppmlib"

def main
=begin
  #test_lightspec
  #test_material
  #test_surface
  #test_solid
  test_plain
  test_sphere
  test_mesh
  test_scene
=end

  graywall = MT[:carpet].color(0.5, 0.5, 0.5)
  bluewall = MT[:carpet].color(0.1, 0.1, 0.4)
  redwall  = MT[:carpet].color(0.4, 0.1, 0.1)

  floor     = Plain.new("floor", graywall, 1.0)
  ceil      = Plain.new("ceil" , graywall, 1.0, [0, -1, 0]).move(0, 4, 0)
  rsidewall = Plain.new("rsidewall" , bluewall, 1.0, [-1, 0, 0]).move(2, 0, 0)
  lsidewall = Plain.new("lsidewall" , redwall, 1.0, [1, 0, 0]).move(-2, 0, 0)
  backwall  = Plain.new("backwall" , graywall, 1.0, [0, 0, 1]).move(0, 0, -6)
  frontwall = Plain.new("frontwall" , graywall, 1.0, [0, 0, -1]).move(0, 0, 5)

  ceiling_light = Mesh.new("ceiling_light", MT[:light], 1.0).scale(1.34).rotate(180, 0, 0).move(0, 3.99, 3)
  ceiling_light.emit(LS[:daywhite], 3500)

  #ball_glass = Sphere.new("ball_glass", MT[:glass], 0.0, 0.7).move(1, 0.7, 2.6)
  ball_diamond = Sphere.new("ball_diamond", MT[:diamond], 0.0, 0.7).move(1, 0.7, 2.6)
  ball_silver = Sphere.new("ball_silver", MT[:silver], 0.0, 0.7).move(-0.9, 0.7, 3.8)

  scene = Scene.new
  scene.put(floor, ceil, rsidewall, lsidewall, backwall, frontwall)
  scene.put(ceiling_light)
  scene.put(ball_diamond, ball_silver)

  puts scene.to_s("photon mapping room")
end

main

#---
