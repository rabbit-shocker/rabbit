#!/usr/bin/env ruby

require "optparse"
require "ostruct"
require "fileutils"

require "rabbit/rabbit"

options = OpenStruct.new
options.rabbit = "bin/rabbit"
options.ps2pdf = "ps2pdf14"
options.output_base = "/tmp/rabbit-print"
options.lang_suffixes = ["", "_en"]

opts = OptionParser.new do |opts|
  opts.banner = "#{opts.banner} RD_FILES"

  opts.on("--rabbit [RABBIT]",
          "rabbit path",
          "(#{options.rabbit})") do |rabbit|
    options.rabbit = rabbit
  end

  opts.on("--ps2pdf [PS2PDF]",
          "ps2pdf path",
          "(#{options.ps2pdf})") do |ps2pdf|
    options.ps2pdf = ps2pdf
  end

  opts.on("--output-base [BASE]",
          "output base directory",
          "(#{options.output_base})") do |base|
    options.output_base = base
  end

  opts.on("--lang_suffixes [SUFFIX,SUFFIX,...]",
          Array,
          "([#{options.lang_suffixes.join(', ')}])") do |suffixes|
    options.lang_suffixes = suffixes
  end
end

opts.parse!(ARGV)

version = `#{options.rabbit} --version`.chop

ARGV.each do |rd|
  base_name = File.basename(rd, ".rd")
  output_dir = File.join(options.output_base, base_name)
  FileUtils.mkdir_p(output_dir)
  puts("processing #{rd}...")
  options.lang_suffixes.each do |lang|
    target_rd = File.join(File.dirname(rd), "#{base_name}#{lang}.rd")
    ps = File.join(output_dir, "#{base_name}#{lang}_#{version}.ps")
    pdf = ps.gsub(/\.ps\z/, ".pdf")
    index_ps = File.join(output_dir, "#{base_name}#{lang}_index_#{version}.ps")
    index_pdf = index_ps.gsub(/\.ps\z/, ".pdf")
    system(options.rabbit,
           "-I", File.dirname(rd),
           "-p",
           "-o", ps,
           target_rd)
    puts("finished #{target_rd}.")
    system(options.ps2pdf, ps, pdf)
    system(options.rabbit,
           "-I", File.dirname(rd),
           "-p",
           "-o", index_ps,
           "--slides-per-page", "8",
           target_rd)
    puts("finished #{target_rd}. (index)")
    system(options.ps2pdf, index_ps, index_pdf)
  end
end
