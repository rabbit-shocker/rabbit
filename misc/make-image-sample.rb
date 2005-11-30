#!/usr/bin/env ruby

require "optparse"
require "ostruct"
require "fileutils"

options = OpenStruct.new
options.rabbit = "bin/rabbit"
options.output_base = "/tmp/rabbit"

opts = OptionParser.new do |opts|
  opts.banner = "#{opts.banner} RD_FILES"
  
  opts.on("--rabbit [RABBIT]",
          "rabbit path",
          "(#{options.rabbit})") do |rabbit|
    options.rabbit = rabbit
  end

  opts.on("--output-base [BASE]",
          "output base directory",
          "(#{options.output_base})") do |base|
    options.output_base = base
  end
end

opts.parse!(ARGV)

ARGV.each do |rd|
  base_name = File.basename(rd, ".rd")
  output_dir = File.join(options.output_base, base_name)
  FileUtils.mkdir_p(output_dir)
  puts("processing #{rd}...")
  system(options.rabbit,
         "-s",
         "-b", File.join(output_dir, base_name),
         "-i", "jpg",
         "-I", File.dirname(rd),
         rd)
end
