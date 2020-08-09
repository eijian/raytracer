#!/usr/bin/ruby

radius0 = 0.5
iteration = 1000
alpha = 0.5

r = radius0
iteration.times do |i|
  puts "#{i} #{r}"
  r = Math.sqrt((i + alpha) / (i + 1.0)) * r
end

puts "reverse---"

r = Math.sqrt((1000 + 1.0) / (1000 + alpha)) * r

(iteration - 1).downto(0) do |i|
  puts "#{i} #{r}"
  r = Math.sqrt(((i - 1) + 1.0) / ((i - 1) + alpha)) * r
end
