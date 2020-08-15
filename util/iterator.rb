#!/usr/bin/ruby
#
# iterator
#

require 'logger'

USAGE = 'iterator.rb <#photon> <#iteration> <radius(m)> <screen(.scr)> <scene(.scene)>'

CMD = "cabal new-exec "
PM = "#{CMD}pm"
RT = "#{CMD}rt"

TMPDIR = "/tmp/"
IMGF   = "tmpimage-" + Time.now.strftime("%Y%m%d%H%M%S-")
ALPHA  = 0.5
NPARA  = 4            # 並列度

def init
  if ARGV.size != 5
    STDERR.puts USAGE
    exit 1
  end

  @nphoton  = ARGV[0].to_i
  @niterate = ARGV[1].to_i
  @radius0  = ARGV[2].to_f
  @screen   = ARGV[3]
  @scene    = ARGV[4]
  @nimage   = @niterate / NPARA   # 1スレッドで生成する画像数
  @nimage  += 1 if @niterate % NPARA != 0
  @radius   = Array.new
  r = @radius0
  @niterate.times do |i|
    @radius[i] = r
    r = Math.sqrt(((i+1) + ALPHA) / ((i+1) + 1.0)) * r
  end

  @tmpdir = TMPDIR + "#{Time.now.strftime("%Y%m%d%H%M%S")}/"
  Dir.mkdir(@tmpdir)
  puts "TMPDIR=#{@tmpdir}"
  @tstart = Time.now

  @logger = Logger.new(@tmpdir + 'iterator.log')
  @logger.datetime_format = '%Y%m%d-%H%M%S'
  @logger.formatter = proc do |severity, datetime, progname, msg|
    "#{datetime} (#{severity}): #{msg}\n"
  end
  @logger.info('Iteration start')
  @logger.info("DIR:#{@tmpdir}")
  @logger.info("PARAM: #photon=#{@nphoton}/#iteration=#{@niterate}/radius=#{@radius0}")
  @logger.info("SCENE: #{@scene} (#{@screen})")
end

def postscript
  elapsed = Time.now - @tstart
  msg = "ELAPSED TIME: #{sprintf("%.2f sec", elapsed.to_f)}"
  STDERR.puts msg
  @logger.info(msg)
  @logger.close
end

def mk_tmpscreen(r, n)
  return if r == nil
  tscrf = @tmpdir + "tmp-#{n}.screen"
  File.open(tscrf, 'w') do |ofp|
    body = ""
    File.open(@screen) do |ifp|
      body = ifp.readlines
    end
    body.each do |l|
      case l
      when /^nphoton/
        ofp.puts "nphoton       : #{@nphoton}"
      when /^progressive/
        ofp.puts "progressive   : yes"
      when /^estimateradius/
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

def iterate(n)
  @nimage.times do |i|
    r = i * NPARA + n
    next if @radius[r] == nil
    msg = "(#{r}) R=#{sprintf("%.4f", @radius[r])}"
    STDERR.puts "#{Time.now.strftime("%Y%m%d-%H%M%S")}: #{msg}"
    @logger.info(msg)
    tscrf = mk_tmpscreen(@radius[r], n)
    mk_image(r, tscrf)
  end
end

def main
  init

  threads = []
  NPARA.times do |p|
    threads.push(Thread.new {iterate(p)})
  end
  threads.each do |t|
    t.join
  end
  postscript
end

main

