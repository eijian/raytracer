#!/usr/bin/ruby

radius0 = 0.5
iteration = 1000
alpha = 0.5

r = radius0 * radius0
iteration.times do |i|
  puts "#{i} #{r}"
  r = (i + alpha) / (i + 1.0) * r

end

