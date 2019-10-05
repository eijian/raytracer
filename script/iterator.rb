#!/usr/bin/ruby
#
# iterator
#

USAGE = 'iterator.rb <#iteration> <#photon> <initial radius(m)> <screen(.scr)> <scene(.scene)>'
PM = "./dist/build/pm/pm"
RT = "./dist/build/rt/rt"
TMPDIR = "/tmp/"
IMGF   = "tmpimage-"
ALPHA  = 0.5

def init
  if ARGV.size != 5
    STDERR.puts USAGE
    exit 1
  end

  @niterate = ARGV[0].to_i
  @nphoton  = ARGV[1].to_i
  @radius0  = ARGV[2].to_f
  @screen   = ARGV[3]
  @scene    = ARGV[4]

  @tmpdir = TMPDIR + "#{Time.now.strftime("%Y%m%d%H%M%S")}/"
  Dir.mkdir(@tmpdir)
end

def postscript

end

def mk_tmpscreen(r)
  tscrf = @tmpdir + "tmp.screen"
  File.open(tscrf, 'w') do |ofp|
    body = ""
    File.open(@screen) do |ifp|
      body = ifp.readlines
    end
    body.each do |l|
      if l =~ /^nphoton/
        ofp.puts "nphoton       : #{@nphoton}"
      elsif l =~ /^estimateradius/
        ofp.puts "estimateradius: #{r}"
      else
        ofp.puts l
      end
    end
  end
  tscrf
end

def mk_image(i, tscrf)
  ppmf = @tmpdir + IMGF + "#{sprintf("%04d", i)}.ppm"
  cmd = "#{PM} #{tscrf} #{@scene} | #{RT} #{tscrf} #{@scene} > #{ppmf}"
  system cmd
end

def main
  init

  r = @radius0 * @radius0
  @niterate.times do |i|
    STDERR.puts "IT: #{i}/#{Time.now.strftime("%Y%m%d-%H%M%S")}"
    tscrf = mk_tmpscreen(r)
    mk_image(i, tscrf)
    r = (i + ALPHA) / (i + 1.0) * r
  end

  postscript
end

main

