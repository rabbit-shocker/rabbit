#!/usr/bin/env ruby

require "optparse"
require "ostruct"
require "fileutils"

require "rabbit/rabbit"

options = OpenStruct.new
options.rabbit = "bin/rabbit"
options.output_base = "/tmp/rabbit-print"
options.lang_suffixes = ["", "_en"]
options.locale_dir = "data/locale"

opts = OptionParser.new do |opts|
  opts.banner = "#{opts.banner} RD_FILES"

  opts.on("--rabbit=RABBIT",
          "rabbit path",
          "(#{options.rabbit})") do |rabbit|
    options.rabbit = rabbit
  end

  opts.on("--output-base=BASE",
          "output base directory",
          "(#{options.output_base})") do |base|
    options.output_base = base
  end

  opts.on("--lang_suffixes=SUFFIX,SUFFIX,...",
          Array,
          "([#{options.lang_suffixes.join(', ')}])") do |suffixes|
    options.lang_suffixes = suffixes
  end

  opts.on("--locale-dir=DIR",
          "locale directory",
          "(#{options.locale_dir})") do |dir|
    options.locale_dir = dir
  end
end

opts.parse!(ARGV)

version = `#{options.rabbit} --version`.chop

def print(rabbit, locale_dir, rd, output, include_path, type)
  args = rabbit.split
  args.concat(["--locale-dir", locale_dir,
               "-I", include_path,
               "-p",
               "-o", output,
               rd])
  system(*args)
  puts("finished #{rd}. (#{type})")
end

def print_index(rabbit, locale_dir, rd, output, include_path, type)
  args = rabbit.split
  args.concat(["--locale-dir", locale_dir,
               "-I", include_path,
               "-p",
               "-o", output,
               "--slides-per-page", "8",
               rd])
  system(*args)
  puts("finished #{rd}. (index #{type})")
end

ARGV.each do |rd|
  base_name = File.basename(rd, ".rd")
  output_dir = File.join(options.output_base, base_name)
  FileUtils.mkdir_p(output_dir)
  puts("processing #{rd}...")
  options.lang_suffixes.each do |lang|
    base_dir = File.dirname(rd)
    target_rd = File.join(base_dir, "#{base_name}#{lang}.rd")
    ps = File.join(output_dir, "#{base_name}#{lang}_#{version}.ps")
    pdf = ps.gsub(/\.ps\z/, ".pdf")
    index_ps = File.join(output_dir, "#{base_name}#{lang}_index_#{version}.ps")
    index_pdf = index_ps.gsub(/\.ps\z/, ".pdf")

    args = [options.rabbit, options.locale_dir, target_rd]
    begin
      original_lang = ENV["LANG"]
      ENV["LANG"] = "C" if /_en\b/ =~ lang
      print(*(args + [ps, base_dir, "PS"]))
      print(*(args + [pdf, base_dir, "PDF"]))
      print_index(*(args + [index_ps, base_dir, "PS"]))
      print_index(*(args + [index_pdf, base_dir, "PDF"]))
    ensure
      ENV["LANG"] = original_lang
    end
  end
end
