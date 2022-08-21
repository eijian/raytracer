#/usr/bin/ruby
#

require 'ebnf'

ebnf = EBNF.parse(File.open(ARGV[0]))

puts ebnf.to_sxp
puts ebnf.to_html
