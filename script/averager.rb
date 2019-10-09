#!/usr/bin/ruby
#
# averager
#

RADIUS = 1.0
ALPHA  = 0.5

def init
  $BASENAME = ARGV.shift

  @colors = Array.new
  @count  = Array.new
  @nfile = 0

  @tarea = 0.0
  @area = Array.new
  r = RADIUS * RADIUS
  1000.times do |i|
    @tarea += r
    @area[i] = r
    r = (i + ALPHA) / (i + 1.0) * r
  end
end

def add_pixel(pos, px, idx)
  #puts "#{pos}, #{px}"
  @colors[pos] = [0, 0, 0] if @colors[pos] == nil
  @count[pos]  = 0 if @count[pos] == nil

  r = px[0].to_f * @area[idx]
  g = px[1].to_f * @area[idx]
  b = px[2].to_f * @area[idx]
  if r > 0 || g > 0 || b > 0
      @colors[pos] = [@colors[pos][0] + r, @colors[pos][1] + g, @colors[pos][2] + b]
      @count[pos] += 1
  end
end

def add_ppm(fn, idx)
  @nfile += 1
  STDERR.puts "(#{idx}): #{fn}"
  File.open(fn) do |fp|
    @hd1 = fp.readline.chomp
    @hd2 = fp.readline.chomp
    @hd3 = fp.readline.chomp.split
    @hd4 = fp.readline.chomp.to_i
    (@hd3[0].to_i * @hd3[1].to_i).times do |pos|
      add_pixel(pos, fp.readline.chomp.split, idx)
    end
  end
end

def clip(c)
  c2 = if c > 255.0 then 255 else c.to_i end
end

def output_ppm
  puts @hd1
  puts @hd2
  puts "#{@hd3[0]} #{@hd3[1]}"
  puts @hd4
  @colors.each_with_index do |c, i|
    if @count[i] == 0
      puts "0 0 0"
    else
      #puts "#{c[0]/ @count[i]} #{c[1] / @count[i]} #{c[2] / @count[i]}"
      #uts "#{c[0]/ @nfile} #{c[1] / @nfile} #{c[2] / @nfile}"
      puts "#{clip(c[0])} #{clip(c[1])} #{clip(c[2])}"
    end
  end
end

def main
  init

  Dir.glob($BASENAME + "*.ppm").sort.each_with_index do |f, i|
    add_ppm(f, i)
  end

  output_ppm
end

main

