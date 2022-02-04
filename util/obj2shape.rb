#!/usr/bin/ruby

USAGE = 'Usage: obj2shape.rb <obj file>'

class Shape

  def self.read_obj(fname)
    grp = 'object001'
    vtxs = Array.new
    norms = Array.new
    norms << [0.0, 0.0, 0.0]
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
          vtxs << [$1.to_f, $3.to_f, $2.to_f, 1.0]
        when 'f'
          dat =~ /^\s*(\S+)\s+(\S+)\s+(\S+)/
          p1 = decode_vtx($1)
          p2 = decode_vtx($2)
          p3 = decode_vtx($3)
          pats << [p1, p2, p3]
        end
      end
    end
    new(grp, vtxs, norms, pats)
  end

  def initialize(grp, vtxs, norms, pats)
    @grp = grp
    @vtxs = vtxs
    @norms = norms
    @pats = pats
    @pats.each_with_index do |p, i|
      @pats[i] = calc_normal(p[0], p[1], p[2])
    end
  end

  def to_s
    body  = "    vtxs_#{@grp} = listArray (0, #{@vtxs.size - 1})\n"
    body += "      [ "
    vtxs_str = @vtxs.map {|v| vector_to_s(v)}
    body += vtxs_str.join("\n      , ") + "\n"
    body += "      ]\n"
    body += "    \n"
    body += "    norms_#{@grp} = listArray (0, 0)\n"
    body += "      [ "
    @pats.each_with_index do |p, i|
      @norms[i+1] = recalc_normal(p[0], p[1], p[2])
    end
    norms_str = @norms.map {|v| vector_to_s(v)}
    body += norms_str.join("\n      , ") + "\n"
    body += "      ]\n"
    body += "    \n"
    body += "    sh_#{@grp} = Mesh (V.fromList\n"
    body += "      [ "
    pats_str = @pats.map {|p| patch_to_s(p)}
    body += pats_str.join("\n      , ") + "\n"
    body += "      ]) vtxs_#{@grp} norms_#{@grp}"

    body
  end

  def scale(s)
    @vtxs = @vtxs.map {|v| [v[0] * s, v[1] * s, v[2] * s]}
  end

  def move(x, y, z)
    @vtxs = @vtxs.map {|v| [v[0] + x, v[1] + y, v[2] + z]}
  end

  def rotate(x, y, z)
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

    @vtxs = @vtxs.map {|v| self.class.matrix_vector(rotx, v)}
    @vtxs = @vtxs.map {|v| self.class.matrix_vector(roty, v)}
    @vtxs = @vtxs.map {|v| self.class.matrix_vector(rotz, v)}

    @norms = @norms.map {|n| self.class.matrix_vector(rotx, n)}
    @norms = @norms.map {|n| self.class.matrix_vector(roty, n)}
    @norms = @norms.map {|n| self.class.matrix_vector(rotz, n)}
    
  end

private

  def self.decode_vtx(v)
    v2 = v.split('/')
    [v2[0].to_i - 1, nil]
  end

  def self.matrix_vector(m, v)
    [inner(m[0], v), inner(m[1], v), inner(m[2], v)]
  end

  def self.inner(v1, v2)
    v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2]
  end

  def calc_normal(p1, p2, p3)
    return p1, p2, p3 if p1[1] != nil || p2[1] != nil || p3[1] != nil
    d1x = @vtxs[p2[0]][0] - @vtxs[p1[0]][0]
    d1y = @vtxs[p2[0]][1] - @vtxs[p1[0]][1]
    d1z = @vtxs[p2[0]][2] - @vtxs[p1[0]][2]
    d2x = @vtxs[p3[0]][0] - @vtxs[p1[0]][0]
    d2y = @vtxs[p3[0]][1] - @vtxs[p1[0]][1]
    d2z = @vtxs[p3[0]][2] - @vtxs[p1[0]][2]
    n0 = [d1y * d2z - d2y * d1z, d1z * d2x - d2z * d1x, d1x * d2y - d2x * d1y]
    len = Math.sqrt(n0[0] * n0[0] + n0[1] * n0[1] + n0[2] * n0[2])
    nv = [n0[0] / len, n0[1] / len, n0[2] / len]
    idx = @norms.size
    @norms << nv
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
    [n0[0] / len, n0[1] / len, n0[2] / len]
  end

  def vector_to_s(v)
    "Vector3 #{float_to_s(v[0])} #{float_to_s(v[1])} #{float_to_s(v[2])}"
  end

  def patch_to_s(p)
    "(#{index_to_s(p[0])}, #{index_to_s(p[1])}, #{index_to_s(p[2])})"
  end

  def float_to_s(f)
    if f <= 0.0 then "(#{f})" else f.to_s end
  end

  def index_to_s(i)
    "(#{i[0]}, #{if i[1] == nil then "0" else i[1] end})"
  end
end



def main
  if ARGV.size != 1 || File.exist?(ARGV[0]) == false
    STDERR.puts USAGE
    exit 1
  end

  shape = Shape.read_obj(ARGV[0])
  # icosahedron
  #shape.scale(1.0 / 0.850651)
  #shape.move(1.0, 1.1, 2.6)

  # octahedron
  #shape.rotate(0.0, 0.0, 50.0)
  #shape.scale(0.7)
  #shape.move(1.0, 0.8, 2.6)

  # sun plane
  shape.scale(5)
  shape.rotate(180, 0, 0)
  shape.move(0, 100, 0)

  # sun plane
  #shape.rotate(0, 45, 0)

  puts shape.to_s
end

main
