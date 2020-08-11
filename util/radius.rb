#!/usr/bin/ruby

if ARGV.size != 2
  STDERR.puts "Usage: radius.rb <#iteration> <radius>"
  exit 1
end

iteration = ARGV[0].to_i
radius0 = ARGV[1].to_f
alpha = 0.5

r = radius0
iteration.times do |i|
  puts "#{i} #{r}"
  r = Math.sqrt((i + alpha) / (i + 1.0)) * r
end
