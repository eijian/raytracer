#!/usr/bin/ruby
#
# iterator
#

require 'logger'

USAGE = 'iterator.rb <#iteration> <#photon> <final radius(m)> <screen(.scr)> <scene(.scene)>'
#PM = "./dist/build/pm/pm"
PM = "cabal run pm"
#RT = "./dist/build/rt/rt"
RT = "cabal run rt"
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
  STDERR.puts "TMPDIR=#{@tmpdir}"
  @tstart = Time.now

  @logger = Logger.new(@tmpdir + 'iterator.log')
  @logger.datetime_format = '%Y%m%d-%H%M%S'
  @logger.formatter = proc do |severity, datetime, progname, msg|
    "#{datetime} (#{severity}): #{msg}\n"
  end
  @logger.info('Iteration start')
  @logger.info("DIR:#{@tmpdir}")
  @logger.info("PARAM: #iteration=#{@niterate}/#photon=#{@nphoton}/radius=#{@radius0}")
  @logger.info("SCENE: #{@scene} (#{@screen})")
end

def postscript
  elapsed = Time.now - @tstart
  msg = "ELAPSED TIME: #{sprintf("%.2f sec", elapsed.to_f)}"
  STDERR.puts msg
  @logger.info(msg)
  @logger.close
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
  ppmf = @tmpdir + IMGF + "#{sprintf("%04d", i)}.ppmf"
  cmd = "#{PM} #{tscrf} #{@scene} | #{RT} #{tscrf} #{@scene} > #{ppmf}"
  system cmd
end

def main
  init

  r = @radius0
  (@niterate - 1).downto(0) do |i|
    msg = "(#{i}) R=#{sprintf("%.4f", r)}"
    STDERR.puts "#{Time.now.strftime("%Y%m%d-%H%M%S")}: #{msg}"
    @logger.info(msg)
    tscrf = mk_tmpscreen(r)
    mk_image(i, tscrf)
    #r = Math.sqrt(((i+1) + ALPHA) / ((i+1) + 1.0)) * r
    r = Math.sqrt(((i - 1) + ALPHA) / ((i - 1) + 1.0)) * r
  end

  postscript
end

main

