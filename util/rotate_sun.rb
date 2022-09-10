#/usr/bin/ruby
#
# rotate_sun.rb
#

USAGE = "Usage: rotate_sun.rb <X deg> <Y deg> <xpos> <ypos> <zpos>"

def init
  if ARGV.size != 5
    STDERR.puts USAGE
    exit 1
  end

  xdeg = ARGV.shift.to_f / 180.0 * Math::PI
  ydeg = ARGV.shift.to_f / 180.0 * Math::PI
  pos = [ARGV[0].to_f, ARGV[1].to_f, ARGV[2].to_f]

  return xdeg, ydeg, pos
end

def rotate_x(pos, xdeg)
  cos = Math::cos(-xdeg)
  sin = Math::sin(-xdeg)

  pos2 = []
  pos2[0] = pos[0]
  pos2[1] = cos * pos[1] - sin * pos[2]
  pos2[2] = sin * pos[1] + cos * pos[2]

  pos2
end

def rotate_y(pos, ydeg)
  cos = Math::cos(-ydeg)
  sin = Math::sin(-ydeg)

  pos2 = []
  pos2[0] = cos * pos[0] + sin * pos[2]
  pos2[1] = pos[1]
  pos2[2] = -sin * pos[0] + cos * pos[2]

  pos2
end

def main
  xdeg, ydeg, pos = init
  pos = rotate_x(pos, xdeg)
  pos = rotate_y(pos, ydeg)
  puts "XD:#{xdeg}, YD:#{ydeg}, POS:#{pos}"
end

main

#---

