#!/usr/bin/env ruby

require 'fileutils'

msgfmt = "rmsgfmt"

podir = "po/"
modir = "data/locale/%s/LC_MESSAGES/"

Dir.glob("#{podir}*/*.po") do |file|
  _, lang, basename = file.sub(/\.po$/, '').split(File::SEPARATOR)
  outdir = modir % lang
  FileUtils.mkdir_p(outdir) unless File.directory?(outdir)
  unless system(msgfmt, file, "-o", "#{outdir}#{basename}.mo")
    STDERR.puts("Can't run: #{msgfmt} #{file} -o #{outdir}#{basename}.mo")
  end
end
