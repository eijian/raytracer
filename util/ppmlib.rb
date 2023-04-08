#!/usr/bin/ruby
#
# ppmlib: PPM modeling language library
#

#
# Utility methods
#

def to_s_2num(a)
  raise StandardError, "Invalid numbers: #{a}" if a.instance_of?(Array) == false && a.size != 3
  "[ #{a[0]}, #{a[1]} ]"
end

def to_s_3num(a)
  raise StandardError, "Invalid numbers: #{a}" if a.instance_of?(Array) == false && a.size != 3
  "[ #{a[0]}, #{a[1]}, #{a[2]} ]"
end

#
# Light Spec
#

class LightSpec

  EST_PHOTON = "photon"
  EST_FORMULA = "formula"
  COL_TEMP = "temperature"
  COL_RGB = "rgb"
  DIR_IN = "in"
  DIR_OUT = "out"

  attr_reader :name

  def initialize(color, directivity, radest, direction = DIR_OUT, radiosity = nil)
    @color, @colortype = check_color(color)
    @directivity = directivity
    @radest = check_radest(radest)
    @direction = direction
    @radiosity = radiosity
    @name = "ls-#{Time.now.strftime("%Y%m%d%H%M%S")}"
  end

  def lumen(lm, area)
    @radiosity = lm / area if @radiosity == nil
    self
  end

  def directivity(directivity)
    @directivity = directivity
    self.clone
  end

  def to_s(name = nil)
    raise StandardError, "Radiosity isn't defined" if @radiosity == nil
    @name = name if name != nil
    col = if @colortype == COL_TEMP
      "temperature: #{@color}"
    else
      "color      : #{to_s_3num(@color)}"
    end
    str = <<-EOS
  #{name}:
    #{col}
    radiosity  : #{@radiosity}
    directivity: #{@directivity}
    rad_est    : #{@radest}
    direction  : #{@direction}
    EOS
    str
  end

private

  def check_color(color)
    if color.kind_of?(Numeric)
      return color, COL_TEMP
    elsif color.instance_of?(Array) && color.size == 3
      return color, COL_RGB
    else
      raise StandardError, "Invalid color setting: #{color}"
    end
  end

  def check_radest(radest)
    if radest == EST_PHOTON || radest == EST_FORMULA
      radest
    else
      raise StandardError, "Invalid type of Radiance Estimation: #{radext}"
    end
  end

end

LS = {
  daylight: LightSpec.new(6500, 0.0, LightSpec::EST_FORMULA),
  daywhite: LightSpec.new(5000, 0.0, LightSpec::EST_FORMULA),
  warmwhite: LightSpec.new(3500, 0.0, LightSpec::EST_FORMULA),
  incandescent: LightSpec.new(2700, 0.0, LightSpec::EST_FORMULA),
}

# TEST

def test_lightspec
  ls = LS[:daywhite].lumen(1500, 2.0)
  puts "LS6500K:"
  #puts ls.to_s("ls_6500k")
  puts ls.to_s("ls_6500k")

  cl = LightSpec.new(2700, 0.0, LightSpec::EST_FORMULA, LightSpec::DIR_OUT, 1950)
  puts cl.to_s("ceiling_light")
end

#
# Material
#

class Material

  BLACK = [0.0, 0.0, 0.0]
  WHITE = [1.0, 1.0, 1.0]

  attr_reader :name

  def initialize(aldiff, scat, metal, trans, ior, alspec = nil)
    @aldiff = aldiff
    @scat = scat
    @metal = metal
    @trans = trans
    @ior = ior
    @alspec = alspec
    @name = "mt-#{Time.now.strftime("%Y%m%d%H%M%S")}"
  end

  def color(r, g, b)
    @aldiff = [r, g, b]
    self.clone
  end

  def trans(r, g, b)
    @trans = [r, g, b]
    self.clone
  end

  def to_s(name = nil)
    @name = name if name != nil
    as = if @alspec == nil then "" else to_s_3num(@alspec) end
    str = <<-EOS
  #{name}:
    albedo_diff  : #{to_s_3num(@aldiff)}
    scatterness  : #{@scat}
    metalness    : #{@metal}
    transmittance: #{to_s_3num(@trans)}
    ior          : #{to_s_3num(@ior)}
    albedo_spec  : #{as}
    EOS
    str
  end

private

end

MT = {
  carpet:    Material.new(Material::WHITE, 1.0, 0.0, Material::BLACK, [1.534, 1.534, 1.534]),
  plastic:   Material.new(Material::WHITE, 1.0, 0.0, Material::BLACK, [2.0, 2.0, 2.0]),
  gypsum:    Material.new(Material::WHITE, 1.0, 0.0, Material::BLACK, [1.52, 1.52, 1.52]),

  glass:     Material.new(Material::WHITE, 0.0, 0.0, Material::WHITE, [1.467, 1.460, 1.455]),
  quartz:    Material.new(Material::WHITE, 0.0, 0.0, Material::WHITE, [1.541, 1.546, 1.554]),
  diamond:   Material.new(Material::WHITE, 0.0, 0.0, Material::WHITE, [2.404, 2.42364, 2.44984]),
  prism_f2:  Material.new(Material::WHITE, 0.0, 0.0, Material::WHITE, [1.61259, 1.623655, 1.643454]),

  gold:      Material.new(Material::BLACK, 0.0, 1.0, Material::BLACK, [0.161, 0.346, 1.562], [0.964, 0.851, 0.392]),
  silver:    Material.new(Material::BLACK, 0.0, 1.0, Material::BLACK, [0.144, 0.124, 0.159], [0.974, 0.960, 0.906]),
  copper:    Material.new(Material::BLACK, 0.0, 1.0, Material::BLACK, [0.216, 0.959, 1.173], [0.980, 0.645, 0.543]),

  air:       Material.new(Material::WHITE, 0.0, 0.0, Material::WHITE, [1.0, 1.0, 1.0]),
  light:     Material.new(Material::WHITE, 0.0, 0.0, Material::WHITE, [1.0, 1.0, 1.0]),
}

def test_material
  dm = MT[:diamond]
  puts dm.to_s("dia")
  puts MT[:gold].to_s("gold")

  pl = MT[:plastic].color(0.5, 0.3, 0.1)
  puts pl.to_s("yellow_plastic")
end

#
# Surface
#

class Surface

  attr_reader :name
  attr_accessor :light

  def initialize(rough, lgt = nil)
    @rough = rough
    @light = lgt
    @name = "sf-#{Time.now.strftime("%Y%m%d%H%M%S")}"
  end

  def to_s(name = nil)
    @name = name if name != nil
    str = <<-EOS
  #{name}:
    roughness: #{@rough}
    light    : #{if @light != nil then @light.name else "" end}
    EOS
    str
  end

end

def test_surface
  sf1 = Surface.new(0.0)
  puts sf1.to_s("surface1")
  puts "SF1: #{sf1.name}"
  sf2 = Surface.new(0.2, LS[:daylight])
  puts sf2.to_s("surface2")
  puts "SF2: #{sf2.name}"
end

#
# Mapper
#

class Mapper

  attr_reader :name

  def initialize
  end

  def set_light(light)
  end
end

class Solid < Mapper

  TYPE = "solid"

  attr_reader :mate, :surf

  def initialize(mate, rough, lgt = nil)
    @mate = mate
    @surf = Surface.new(rough, lgt)
    @name = "sl-#{Time.now.strftime("%Y%m%d%H%M%S")}"
  end

  def to_s(name = nil)
    @name = name if name != nil
    str = <<-EOS
  #{@name}:
    type    : #{TYPE}
    map     : [ #{@mate.name}, #{@surf.name} ]
    EOS
    str
  end

  def set_light(light)
    @surf.light = light
  end

  def materials
    [@mate]
  end

  def surfaces
    [@surf]
  end
end

def test_solid
  sl = Solid.new(MT[:gold], 0.0)
  puts sl.to_s("gold")
end

class Checker < Mapper

  TYPE = "checker"

  def initialize(mates, surfs, s = 1.0)
    @map1 = [mates[0], surfs[0]]
    @map2 = [mates[1], surfs[1]]
    @scale = s
    @name = "ch-#{Time.now.strftime("%Y%m%d%H%M%S")}"
  end

  def to_s(name = nil)
    @name = name if name != nil
    str = <<-EOS
  #{@name}:
    type    : #{TYPE}
    map1    : [ #{@map1[0].name}, #{@map1[1].name} ]
    map2    : [ #{@map2[0].name}, #{@map2[1].name} ]
    scale   : #{@scale}
    EOS
    str
  end

  def set_light(light)
    @map1[1].light = light
    @map2[1].light = light
  end

  def materials
    [@map1[0], @map2[0]]
  end

  def surfaces
    [@map1[1], @map2[1]]
  end
end

#
# Prim
#

class Prim

  attr_reader :map, :name

  def initialize(name, mate = MT[:gypsum], rough = 0.0)
    @name = name
    @map  = Solid.new(mate, rough)
    @light = nil
    @lumen = 0.0
  end

  def move(x, y, z)
    self.clone
  end

  def scale(s)
    self.clone
  end

  def rotate(x, y, z)
    self.clone
  end

  def emit(lgt, lumen)
    @light = lgt
    @lumen = lumen
    @map.set_light(@light)
    self.clone
  end

  def get_light
  end

  def mapping(map)
    @map = map
    self.clone
  end

  def area
    1.0e100
  end

  def materials
    @map.materials
  end

  def surfaces
    @map.surfaces
  end

  def lights
    if @light == nil
      []
    else
      [@light.lumen(@lumen, area)]
    end
  end

protected

  def self.rotate(x, y, z)
    mag = 2 * Math::PI / 360.0
    x = x * mag
    y = y * mag
    z = z * mag
    cosx = Math.cos(x)
    sinx = Math.sin(x)
    cosy = Math.cos(y)
    siny = Math.sin(y)
    cosz = Math.cos(z)
    sinz = Math.sin(z)

    rotx = [[ 1.0 ,  0.0 ,  0.0],
            [ 0.0 ,  cosx, -sinx],
            [ 0.0 ,  sinx,  cosx]]
    roty = [[ cosy,  0.0 ,  siny],
            [ 0.0 ,  1.0 ,  0.0],
            [-siny,  0.0 ,  cosy]]
    rotz = [[ cosz, -sinz,  0.0],
            [ sinz,  cosz,  0.0],
            [ 0.0 ,  0.0 ,  1.0]]

    return rotx, roty, rotz
  end

  def self.matrix_vector(m, v)
    [inner(m[0], v), inner(m[1], v), inner(m[2], v)]
  end

  def self.inner(v1, v2)
    v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2]
  end

end

class Plain < Prim

  TYPE = "plain"

  def initialize(name, mate, rough, nvec = [0.0, 1.0, 0.0])
    @pos = [0.0, 0.0, 0.0]
    @nvec = nvec
    super(name, mate, rough)
  end

  def move(x, y, z)
    @pos = [@pos[0] + x, @pos[1] + y, @pos[2] + z]
    self.clone
  end

  def rotate(x, y, z)
    rotx, roty, rotz = Prim::rotate(x, y, z)
    @pos = Prim::matrix_vector(rotx, @pos)
    @pos = Prim::matrix_vector(roty, @pos)
    @pos = Prim::matrix_vector(rotz, @pos)
    @nvec = Prim::matrix_vector(rotx, @nvec)
    @nvec = Prim::matrix_vector(roty, @nvec)
    @nvec = Prim::matrix_vector(rotz, @nvec)
    self.clone
  end

  def to_s(name = nil)
    @name = name if name != nil
    str = <<-EOS
  #{@name}:
    type    : #{TYPE}
    normal  : #{to_s_3num(@nvec)}
    position: #{to_s_3num(@pos)}
    mapper  : #{@map.name}
    EOS
  end

end

def test_plain
  pl = Plain.new("plain1", MT[:gold], 0.5)
  puts pl.to_s
  pl.move(2.0, 0.0, 0.0)
  puts pl.to_s
  pl.rotate(10.0, 45.0, 60.0)
  puts pl.to_s

end


class Sphere < Prim

  TYPE = "sphere"

  def initialize(name, mate, rough, radius = 1.0)
    @radius = radius
    @center = [0.0, 0.0, 0.0]
    super(name, mate, rough)
  end

  def scale(s)
    @radius = @radius * s
    self.clone
  end

  def move(x, y, z)
    @center = [@center[0] + x, @center[1] + y, @center[2] + z]
    self.clone
  end

  def rotate(x, y, z)
    rotx, roty, rotz = Prim::rotate(x, y, z)
    @center = Prim::matrix_vector(rotx, @center)
    @center = Prim::matrix_vector(roty, @center)
    @center = Prim::matrix_vector(rotz, @center)
    self.clone
  end

  def area
    4.0 * Math::PI * @radius * @radius
  end

  def to_s(name = nil)
    @name = name if name != nil
    str = <<-EOS
  #{@name}:
    type    : #{TYPE}
    center  : #{to_s_3num(@center)}
    radius  : #{@radius}
    mapper  : #{@map.name}
    EOS
  end

end

def test_sphere
  sp = Sphere.new("sphere1", MT[:gold], 0.0)
  puts sp.to_s
  sp.scale(0.4)
  puts sp.to_s
  sp.move(3.0, 0.4, 1.3)
  puts sp.to_s
  sp.rotate(45.0, 0.0, 60.0)
  puts sp.to_s
end

class Mesh < Prim

  FILE_OBJ = "obj"
  TYPE = "mesh"
  UNIT_VTX = [[-0.5, 0.0, -0.5], [0.5, 0.0, -0.5], [0.5, 0.0, 0.5], [-0.5, 0.0, 0.5]]
  UNIT_NVEC = [[0.0, 1.0, 0.0]]
  UNIT_UV = [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0], [1.0, 1.0]]
  UNIT_PAT = [[[0, 0, 0], [1, 0, 0], [2, 0, 0]], [[0, 0, 0], [2, 0, 0], [3, 0, 0]]]

  def self.read_file(name, mate, rough, fname, type = FILE_OBJ)
    grp = 'object001'
    vtxs = Array.new
    nvecs = Array.new
    #nvecs << [0.0, 0.0, 0.0]
    uvs = Array.new
    uvs += [[0.0, 0.0], [1.0, 1.0]]
    pats = Array.new
    File.open(fname, 'r') do |fp|
      fp.each do |l|
        l.chomp =~ /^(\S+) (.*)/
        kw = $1
        dat = $2
        case kw
        when '#'
          next
        when 'g'
          grp = dat.split[0]
        when 'v'
          dat =~ /^\s*(\S+)\s+(\S+)\s+(\S+)/
          vtxs << [$1.to_f, $3.to_f, $2.to_f]
        when 'f'
          dat =~ /^\s*(\S+)\s+(\S+)\s+(\S+)/
          p1 = decode_vtx($1)
          p2 = decode_vtx($2)
          p3 = decode_vtx($3)
          pats << [p1, p2, p3]
        end
      end
    end
    new(name, mate, rough, vtxs, nvecs, uvs, pats)
  end

  def initialize(name, mate = MT[:gypsum], rough = 0.0, vtxs = UNIT_VTX.clone, nvecs = UNIT_NVEC.clone, uvs = UNIT_UV.clone, pats = UNIT_PAT.clone)
    @vtxs = vtxs
    @nvecs = nvecs
    @uvs = uvs
    @pats = pats
    if @nvecs == nil || @nvecs.size == 0
      @pats.each_with_index do |p, i|
        @pats[i] = calc_normal(p[0], p[1], p[2])
      end
    end
    super(name, mate, rough)
  end

  def scale(s)
    @vtxs = @vtxs.map {|v| [v[0] * s, v[1] * s, v[2] * s]}
    self.clone
  end

  def move(x, y, z)
    @vtxs = @vtxs.map {|v| [v[0] + x, v[1] + y, v[2] + z]}
    self.clone
  end

  def rotate(x, y, z)
    rotx, roty, rotz =  Prim::rotate(x, y, z)

    @vtxs = @vtxs.map {|v| Prim::matrix_vector(rotx, v)}
    @vtxs = @vtxs.map {|v| Prim::matrix_vector(roty, v)}
    @vtxs = @vtxs.map {|v| Prim::matrix_vector(rotz, v)}

    @nvecs = @nvecs.map {|n| Prim::matrix_vector(rotx, n)}
    @nvecs = @nvecs.map {|n| Prim::matrix_vector(roty, n)}
    @nvecs = @nvecs.map {|n| Prim::matrix_vector(rotz, n)}

    #@pats.each_with_index do |p, i|
    #  @nvecs[i+1] = recalc_normal(p[0], p[1], p[2])
    #end
    self.clone
  end

  def area
    s = 0
    @pats.each do |p|
      p1 = @vtxs[p[0][0]]
      p2 = @vtxs[p[1][0]]
      p3 = @vtxs[p[2][0]]
      v1 = [p2[0] - p1[0], p2[1] - p1[1], p2[2] - p1[2]]
      v2 = [p3[0] - p1[0], p3[1] - p1[1], p3[2] - p1[2]]
      #STDERR.puts "V1: #{v1}, V2: #{v2}"
      v = cross(v1, v2)
      s0 = Math::sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]) / 2.0
      #STDERR.puts "S0: #{s0}"
      s += s0
    end
    s
  end

  def to_s(name = nil)
    @name = name if name != nil
    vlist = @vtxs.map  {|v| "      - #{to_s_3num(v)}"}
    nlist = @nvecs.map {|n| "      - #{to_s_3num(n)}"}
    ulist = @uvs.map   {|u| "      - #{to_s_2num(u)}"}
    plist = @pats.map  {|p| "      - #{to_s_3num(p)}"}
    str = <<-EOS
  #{@name}:
    type    : #{TYPE}
    vertex  :
#{vlist.join("\n")}
    normal  :
#{nlist.join("\n")}
    uvmap   :
#{ulist.join("\n")}
    polygon :
#{plist.join("\n")}
    mapper  : #{@map.name}
    EOS
  end

private

  def calc_normal(p1, p2, p3)
    return p1, p2, p3 if p1[1] != nil || p2[1] != nil || p3[1] != nil
    #STDERR.puts "P1:#{p1}, P2:#{p2}, P3:#{p3}"
    #STDERR.puts "VTX:#{@vtxs}"
    d1x = @vtxs[p2[0]][0] - @vtxs[p1[0]][0]
    d1y = @vtxs[p2[0]][1] - @vtxs[p1[0]][1]
    d1z = @vtxs[p2[0]][2] - @vtxs[p1[0]][2]
    d2x = @vtxs[p3[0]][0] - @vtxs[p1[0]][0]
    d2y = @vtxs[p3[0]][1] - @vtxs[p1[0]][1]
    d2z = @vtxs[p3[0]][2] - @vtxs[p1[0]][2]
    n0 = [d1y * d2z - d2y * d1z, d1z * d2x - d2z * d1x, d1x * d2y - d2x * d1y]
    len = Math.sqrt(n0[0] * n0[0] + n0[1] * n0[1] + n0[2] * n0[2])
    nv = if len > 0.0 then
      [-n0[0] / len, -n0[1] / len, -n0[2] / len]
    else
      [0.0, 0.0, 0.0]
    end

    idx = @nvecs.size
    #STDERR.puts "NV:#{nv}"
    @nvecs << nv
    p1[1] = idx
    p2[1] = idx
    p3[1] = idx
    [p1, p2, p3]
  end

  def recalc_normal(p1, p2, p3)
    d1x = @vtxs[p2[0]][0] - @vtxs[p1[0]][0]
    d1y = @vtxs[p2[0]][1] - @vtxs[p1[0]][1]
    d1z = @vtxs[p2[0]][2] - @vtxs[p1[0]][2]
    d2x = @vtxs[p3[0]][0] - @vtxs[p1[0]][0]
    d2y = @vtxs[p3[0]][1] - @vtxs[p1[0]][1]
    d2z = @vtxs[p3[0]][2] - @vtxs[p1[0]][2]
    n0 = [d1y * d2z - d2y * d1z, d1z * d2x - d2z * d1x, d1x * d2y - d2x * d1y]
    len = Math.sqrt(n0[0] * n0[0] + n0[1] * n0[1] + n0[2] * n0[2])
    [-n0[0] / len, -n0[1] / len, -n0[2] / len]
  end

  def self.decode_vtx(v)
    v2 = v.split('/')
    t = v2[0].to_i - 1
    n = if v2[1] != nil then v2[1].to_i else nil end
    u = if v2[2] != nil then v2[2].to_i else 0 end
    [t, n, u]
  end

  def cross(v1, v2)
    [v1[1] * v2[2] - v1[2] * v2[1], v1[2] * v2[0] - v1[0] * v2[2], v1[0] * v2[1] - v1[1] * v2[0]]
  end
end

def test_mesh
  ms = Mesh.new("mesh1", MT[:gold], 1.0)
  puts ms.to_s
  puts "AR: #{ms.area}"
  ms.scale(2.0).move(-1.0, 0.0, -1.0)
  puts ms.to_s
  puts "AR: #{ms.area}"
  ms.rotate(45, 0, 0)
  puts ms.to_s
  puts "AR: #{ms.area}"
end

#
# Scene
#

class Scene

  def initialize
    @objects = Array.new
  end

  def put(*objs)
    @objects += objs
  end

  def to_s(title)
    slist = @objects.map {|o| "  - #{o.name}"}
    mps = @objects.map {|o| o.map }
    mts = @objects.map {|o| o.materials}.flatten
    sfs = @objects.map {|o| o.surfaces}.flatten
    lts = @objects.map {|o| o.lights}.flatten
    llist = lts.map.with_index(1) {|lt, i| lt.to_s("lgt_#{idxstr(i)}")}
    mtlist = mts.map.with_index(1) {|mt, i| mt.to_s("mat_#{idxstr(i)}")}
    sflist = sfs.map.with_index(1) {|sf, i| sf.to_s("sf_#{idxstr(i)}")}
    mplist = mps.map.with_index(1) {|mp, i| mp.to_s("map_#{idxstr(i)}")}
    olist = @objects.map.with_index(1) {|o, i| o.to_s}


    str = <<-EOS
#
# TITLE: #{title}
# GENERATE: #{Time.now.strftime("%Y-%m-%d %H:%M:%S")}

lightspec:
#{llist.join("\n")}

material:
#{mtlist.join("\n")}

surface:
#{sflist.join("\n")}

mapper:
#{mplist.join("\n")}

object:
#{olist.join("\n")}

scene:
#{slist.join("\n")}

    EOS
    str
  end

private

  def idxstr(i)
    sprintf("%03d", i)
  end

end

def test_scene
  ycarpet = MT[:carpet].color(0.5, 0.3, 0.1)
  floor = Mesh.new("ycarpet", ycarpet, 1.0).scale(2.0).move(-1.0, 0.0, -1.0)
  ball = Sphere.new("glassball", MT[:glass], 1.0, 0.4).move(0, 0.4, 0)
  bulb = Sphere.new("bulb", MT[:light], 0.0, 0.0475).move(0, 3.5, 0).emit(LS[:daywhite], 1370)

  scene = Scene.new
  scene.put(floor)
  scene.put(ball)
  scene.put(bulb)
  puts scene.to_s("test_scene")
end

#---
