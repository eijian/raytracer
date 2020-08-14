#!/usr/bin/ruby
#
# averager
#

USAGE = "Usage: averager2.rb [-h|-exr|-ppm] [output filename]\n" +
        "  app waits a temporary directory name from STDIN.\n"     +
        "  if o/p file isn't given, image is written to STDOUT."

ALPHA  = 0.5
GAMMA  = 1.0 / 2.2
RGBMAX = 255

FMT_PPM = 0
FMT_EXR = 1

def init
  $FILE = nil
  $FORMAT = FMT_EXR
  if ARGV.size >= 1
    case ARGV[0].chomp
    when '-exr'
      $FILE   = ARGV[1]
    when '-ppm'
      $FORMAT = FMT_PPM
      $FILE   = ARGV[1]
    when '-h'
      STDERR.puts USAGE
      exit 1
    else
      $FILE   = ARGV[0]
    end
  end
  
  @colors = Array.new
  @count  = Array.new
  @nfile = 0
end

def get_base
  bn = ""
  STDIN.each do |l|
    l.chomp =~ /^TMPDIR=(.+)/
    bn = $1 if $1 != ""
  end
  bn
end

def add_pixel(pos, px, idx)
  #puts "#{pos}, #{px}"
  @colors[pos] = [0, 0, 0] if @colors[pos] == nil
  @count[pos]  = 0 if @count[pos] == nil

  r = px[0].to_f
  g = px[1].to_f
  b = px[2].to_f

  if r > 0 || g > 0 || b > 0
      @colors[pos] = [@colors[pos][0] + r, @colors[pos][1] + g, @colors[pos][2] + b]
      @count[pos] += 1
  end
end

def add_image(fn, idx)
  return if FileTest.zero?(fn)
  @nfile += 1
  STDERR.print "(#{idx}): #{fn}\r"
  File.open(fn) do |fp|
    @hd1 = fp.readline.chomp
    fp.readline.chomp =~ /radiance = (.+)$/
    @hd2 = $1.to_f
    @hd3 = fp.readline.chomp.split
    @hd4 = fp.readline.chomp.to_i
    (@hd3[0].to_i * @hd3[1].to_i).times do |pos|
      add_pixel(pos, fp.readline.chomp.split, idx)
    end
  end
end

def clip(c)
  #c2 = if c > 255.0 then 255 else c.to_i end
  c2 = c / @nfile / @hd2
  r = ((if c2 > 1.0 then 1.0 else c2 end) ** GAMMA) * RGBMAX
  begin
    r.to_i
  rescue StandardError => e
    STDERR.puts "C2:#{c2}/R:{r}"
    STDERR.puts e
  end
end

def output_ppm(fp)
  fp.puts @hd1
  fp.puts "## max radiance = #{@hd2}"
  fp.puts "#{@hd3[0]} #{@hd3[1]}"
  fp.puts @hd4
  @colors.each_with_index do |c, i|
    if @count[i] == 0
      fp.puts "0 0 0"
    else
      #puts "#{c[0]/ @count[i]} #{c[1] / @count[i]} #{c[2] / @count[i]}"
      #uts "#{c[0]/ @nfile} #{c[1] / @nfile} #{c[2] / @nfile}"
      fp.puts "#{clip(c[0])} #{clip(c[1])} #{clip(c[2])}"
    end
  end
end

## CONSTANTS FOR OpenEXR
HALF  = 1
FLOAT = 2

NONE  = 0

INCY  = 0
DECY  = 1

INTLEN   = 4
FLOATLEN = 4
QUADLEN  = 8

def i2b(i)
  [i].pack('L')
end

def i2b_a(a)
  ar = ""
  a.each do |i|
    STDERR.puts("AR:#{ar}")
    ar += i2b(i)
  end
  ar
end

def c2b(c)
  [c].pack('c')
end

def q2b(q)
  [q].pack('Q')
end

def f2b(f)
  [f].pack('e')
end

def s2b(s)
  (s + "\0").bytes.pack('C*')
end

def header_exr(xlen, ylen)
  magick   = i2b(20000630)
  version  = i2b(2)
  chname   = s2b("channels") + s2b("chlist")
  ch       = s2b("B") + i2b(FLOAT) + i2b(0) + i2b(1) + i2b(1) +
             s2b("G") + i2b(FLOAT) + i2b(0) + i2b(1) + i2b(1) +
             s2b("R") + i2b(FLOAT) + i2b(0) + i2b(1) + i2b(1) + s2b("")
  chlen    = i2b(ch.size)
  compress = s2b("compression") + s2b("compression") + i2b(1)          + c2b(NONE)
  datawin  = s2b("dataWindow")  + s2b("box2i")       + i2b(INTLEN*4)   +
             [0, 0, xlen - 1, ylen - 1].map{|i| i2b(i)}.inject{|m, x| m + x}
  dispwin  = s2b("displayWindow") + s2b("box2i")     + i2b(INTLEN*4)   +
             [0, 0, xlen - 1, ylen - 1].map{|i| i2b(i)}.inject{|m, x| m + x}
  lineord  = s2b("lineOrder") + s2b("lineOrder")     + i2b(1)          + c2b(INCY)
  pxaspect = s2b("pixelAspectRatio") + s2b("float")  + i2b(FLOATLEN)   + f2b(1.0)
  scenter  = s2b("screenWindowCenter") + s2b("v2f")  + i2b(FLOATLEN*2) + f2b(0.0) + f2b(0.0)
  swidth   = s2b("screenWindowWidth") + s2b("float") + i2b(FLOATLEN)   + f2b(1.0)
  
  header   = magick + version + 
             chname + chlen + ch +
             compress + datawin + dispwin + lineord + pxaspect + scenter + swidth +
             s2b("")
end

def output_exr(fp)

  xlen = @hd3[0].to_i
  ylen = @hd3[1].to_i
  cmag  = @nfile * @hd2

  header = header_exr(xlen, ylen)
  headerlen = header.size
  offset    = headerlen + QUADLEN * ylen
  datalen   = FLOATLEN * xlen * 3
  linelen   = INTLEN * 2 + datalen

  fp.write(header)
  ylen.times do |y|
      fp.write(q2b(offset + linelen * y))
  end
  ylen.times do |y|
    rval = Array.new
    gval = Array.new
    bval = Array.new

    xlen.times do |x|
      pos = y * xlen + x
      if @count[pos] == 0
        rval << 0.0
        gval << 0.0
        bval << 0.0
      else
        c = @colors[pos]
        rval << c[0] / cmag
        gval << c[1] / cmag
        bval << c[2] / cmag
      end
    end

    line = bval.map{|c| f2b(c)}.inject{|m, x| m + x} +
           gval.map{|c| f2b(c)}.inject{|m, x| m + x} +
           rval.map{|c| f2b(c)}.inject{|m, x| m + x}
    fp.write(i2b(y) + i2b(datalen) + line)
  end 
end

def open_file
  fp = STDOUT
  if $FILE != nil
    fp = File.open($FILE, 'wb')
  end
  fp
end

def close_file(fp)
  fp.close
end

def main
  init

  basename = get_base
  Dir.glob(basename + "*.ppmf").sort.reverse.each_with_index do |f, i|
    add_image(f, i)
  end

  fp = open_file
  begin
    if $FORMAT == FMT_PPM
      output_ppm(fp)
    else
      output_exr(fp)
    end
  rescue
    close_file(fp)
  end
end

main


