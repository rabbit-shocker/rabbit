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
  original_base_name = File.basename(rd, ".rd")
  themes = [nil]
  case original_base_name
  when "rabbit"
    themes << "red-frame"
  when "lightning-talk"
    themes << "lightning-talk-with-contact"
  end
  themes.each do |theme|
    base_name = original_base_name.dup
    base_name << "-#{theme.sub(/^#{base_name}-/, '')}" if theme
    output_dir = File.join(options.output_base, base_name)
    FileUtils.mkdir_p(output_dir)
    puts("processing #{rd}...")
    args = [
      "-s",
      "-b", File.join(output_dir, base_name),
      "-i", "jpg",
      "-I", File.dirname(rd),
    ]
    args.concat(["-t", theme]) if theme
    args << rd
    system(*(options.rabbit.split + args))
    system(*(options.rabbit.split + ["--index-mode"] + args))
  end
end
