#/usr/bin/ruby
#
# calc complex realm
#

n1 = Complex(1.0, 0.0)  # 真空の屈折率（複素数）

print "Red: "
rs = gets.chomp.split
print "Green: "
gs = gets.chomp.split
print "Blue: "
bs = gets.chomp.split

nr = Complex(rs[0].to_f, rs[1].to_f)
ng = Complex(gs[0].to_f, gs[1].to_f)
nb = Complex(bs[0].to_f, bs[1].to_f)

r = ((n1 - nr) / (n1 + nr)) ** 2
g = ((n1 - ng) / (n1 + ng)) ** 2
b = ((n1 - nb) / (n1 + nb)) ** 2

puts "R: #{r.abs}, G: #{g.abs}, B: #{b.abs}"

# AU R: 0.9643081147440908, G: 0.8507024267435965, B: 0.3915946384374889
# AG R: 0.9742168040905042, G: 0.9602324477569816, B: 0.9191458589962683
# CU R: 0.9555926895698736, G: 0.6359552094617925, B: 0.553538452075658
#--

=begin

cf. https://docs.arnoldrenderer.com/pages/viewpage.action?pageId=38175887

aluminium: 0.500 0.492 0.473
chrome: 0.543 0.543 0.566
copper: 0.980 0.645 0.543
gold: 0.984 0.785 0.344
platinum: 0.676 0.637 0.578
silver: 0.965 0.953 0.918
titanium: 0.547 0.496 0.445
tungsten: 0.500 0.492 0.473

=end
