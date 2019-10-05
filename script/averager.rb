#!/usr/bin/ruby
#
# averager
#


def init
  $BASENAME = ARGV.shift

  @colors = Array.new
  @count  = Array.new
  @nfile = 0
end

def add_pixel(i, px)
  #puts "#{i}, #{px}"
  @colors[i] = [0, 0, 0] if @colors[i] == nil
  @count[i]  = 0 if @count[i] == nil

  r = px[0].to_i
  g = px[1].to_i
  b = px[2].to_i
  if r > 0 || g > 0 || b > 0
      @colors[i] = [@colors[i][0] + r, @colors[i][1] + g, @colors[i][2] + b]
      @count[i] += 1
  end
end

def add_ppm(fn)
  @nfile += 1
  STDERR.puts "FN:#{fn}"
  File.open(fn) do |fp|
    @hd1 = fp.readline.chomp
    @hd2 = fp.readline.chomp
    @hd3 = fp.readline.chomp.split
    @hd4 = fp.readline.chomp.to_i
    (@hd3[0].to_i * @hd3[1].to_i).times do |i|
      add_pixel(i, fp.readline.chomp.split)
    end
  end
end

def clip(c)
  c2 = if c > 255 then 255 else c end
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
      puts "#{c[0]/ @count[i]} #{c[1] / @count[i]} #{c[2] / @count[i]}"
      #uts "#{c[0]/ @nfile} #{c[1] / @nfile} #{c[2] / @nfile}"
      #puts "#{clip(c[0])} #{clip(c[1])} #{clip(c[2])}"
    end
  end
end

def main
  init

  Dir.glob($BASENAME + "*.ppm").each do |f|
    add_ppm(f)
  end

  output_ppm
end

main


