#!/usr/bin/ruby
#
# iterator 2nd ver.
#

require 'logger'

USAGE = 'iterator2.rb <#photon> <#iteration> <radius(m)> <screen(.scr)> <scene(.scene)>'

CMD = "cabal new-exec "
PM = "#{CMD}pm"
RT = "#{CMD}rt"

TMPDIR = "/tmp/"
IMGF   = "tmpimage-" + Time.now.strftime("%Y%m%d%H%M%S-")
ALPHA  = 0.5
NPARA  = 4            # 並列度

def read_conf(scr)
  x = 0
  y = 0
  r = 1.0
  File.open(scr) do |fp|
    fp.each do |l|
      #STDERR.puts "LINE: #{l}"
      case l
      when /xresolution\s*: (\d+)/
        x = $1.to_i
      when /yresolution\s*: (\d+)/
        y = $1.to_i
      when /maxradiance\s*: (\S+)/
        r = $1.to_f
      end
    end
  end
  return x, y, r
end

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
  @mut = Mutex.new
  @xreso, @yreso, @radiance = read_conf(@screen)
  @pixels = Array.new(NPARA) do |i|
    Array.new(@xreso * @yreso, [0.0, 0.0, 0.0])
  end

  @tmpdir = TMPDIR + "#{Time.now.strftime("%Y%m%d%H%M%S")}/"
  Dir.mkdir(@tmpdir)
  puts "TMPDIR=#{@tmpdir}"
  puts "NITERATION=#{@niterate}"
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

def next_radius
  @mut.synchronize {
    rad = @radius.shift
    return rad, @radius.length
  }
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

def mk_image(n, tscrf)
  #ppmf = @tmpdir + IMGF + "#{sprintf("%04d", i)}.ppmf"
  cmd = "#{PM} #{tscrf} #{@scene} | #{RT} #{tscrf} #{@scene}"
  ofs = 0
  `#{cmd}`.each_line do |l|
    case l
    when 'P3'
      next
    when /^#/
      next
    when /^\d+ \d+\s*$/
      next
    when /^\d+\s*$/
      next
    when /^(\S+) (\S+) (\S+)\s*$/
      p = [$1.to_f, $2.to_f, $3.to_f]
      p0 = @pixels[n][ofs]
      @pixels[n][ofs] = [p0[0] + p[0], p0[1] + p[1], p0[2] + p[2]]
      ofs += 1
    end
  end
end

def output_image(n)
  ppmf = @tmpdir + IMGF + "#{sprintf("%04d", n)}.ppmf"
  File.open(ppmf, 'w') do |fp|
    fp.puts "P3"
    fp.puts "## max radiance = #{@radiance}"
    fp.puts "#{@xreso} #{@yreso}"
    fp.puts "255"
    @pixels[n].each do |p|
      fp.puts p.join(' ')
    end
  end
end

def iterate(n)
  loop do
    rad, i = next_radius
    break if rad == nil
    msg = "(#{n}/#{i}) R=#{sprintf("%.4f", rad)}"
    elaps = Time.now - @tstart
    nite = @niterate - (i + 1)
    timeleft = if nite == 0 then 0 else (elaps.to_f / nite.to_f) * (i + 1).to_f end
    #STDERR.puts "#{Time.now.strftime("%Y%m%d-%H%M%S")}: #{msg}"
    STDERR.puts "#{sprintf("%5d s / %5d s", elaps, timeleft)} : #{msg}"
    @logger.info(msg)
    tscrf = mk_tmpscreen(rad, n)
    mk_image(n, tscrf)
  end
  output_image(n)
end

def main
  init

  STDERR.puts "ELAPSED / REMAIN  : (TH/ITE) RADIUS"
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

