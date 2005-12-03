#!/usr/bin/env ruby

require 'fileutils'

msgfmt = "ruby -S rmsgfmt"

podir = "po/"
modir = "data/locale/%s/LC_MESSAGES/"

Dir.glob("#{podir}*/*.po") do |file|
  _, lang, basename = file.sub(/\.po$/, '').split(File::SEPARATOR)
  outdir = modir % lang
  FileUtils.mkdir_p(outdir) unless File.directory?(outdir)
  command = "#{msgfmt} #{file} -o #{outdir}#{basename}.mo"
  unless system(command)
    STDERR.puts("Can't run: #{command}")
  end
end
