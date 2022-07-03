#!/usr/bin/ruby

def area(i)
  a = -Math.cos((i+1) * $theta) - (-Math.cos(i * $theta))
  2.0 * Math::PI * a
end

def main
  ndiv = STDIN.gets.to_i
  nphoton = STDIN.gets.to_i
  n = STDIN.gets.to_f
  STDERR.puts "NDIV=#{ndiv}/NPHOTON=#{nphoton}/N=#{n}"
  STDERR.puts "1/2 PI =#{1.0 / Math::PI / 2.0}"

  $theta = Math::PI / 2.0 / ndiv.to_f
  dist = Array.new

  STDIN.each do |l|
    dist[l.to_i] = 0 if dist[l.to_i] == nil
    dist[l.to_i] += 1
  end

  total = 0.0
  print "#{n},"
  dist.each_with_index do |d, i|
    a = area(i)
    d2 = d.to_f / nphoton.to_f
    total += d2
    #puts "D[#{i}] = #{d2} / A=#{a} -> #{d2 / a}"
    print "#{d2 / a},"
  end
  puts ''

  STDERR.puts "TOTAL=#{total}"
end

main

# last
