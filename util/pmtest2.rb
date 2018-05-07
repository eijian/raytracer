#!/usr/bin/ruby
#


@map = Array.new
XRES = 2048
YRES = 2048

def color(c, oldc)
  oc = if oldc != nil then oldc else 0 end
  cc = case c
    when "Red"   then 1
    when "Green" then 2
    when "Blue"  then 4
    end
  cc | oc
end

def putImg
  puts "P3"
  puts "## test"
  puts "#{XRES} #{YRES}"
  puts "255"
  0.upto(XRES * YRES) do |i|
    c = if @map[i] != nil then @map[i] else 0 end
    cc = case c
      when 0 then "0 0 0"
      when 1 then "255 0 0"
      when 2 then "0 255 0"
      when 3 then "255 255 0"
      when 4 then "0 0 255"
      when 5 then "255 0 255"
      when 6 then "0 255 255"
      when 7 then "255 255 255"
      end
    puts cc
  end
end

def main
  STDIN.each do |l|
    /\((.+),(\d+),(\d+)\)/ =~ l
    c = $1
    p = $3.to_i * YRES + $2.to_i
    cc = color(c, @map[p])
    @map[p] = cc
    #puts "C=#{c},CC=#{cc},P=#{p}"
  end

  putImg
end

main
