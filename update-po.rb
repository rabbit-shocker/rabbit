#!/usr/bin/env ruby

require "fileutils"

gettext = "rgettext"
msgmerge = "msgmerge"

podir = "po/"
appname = "rabbit"
pot = "#{podir}#{appname}.pot"
targets = Dir.glob("lib/**/*.rb") + Dir.glob("bin/*")

FileUtils.rm_f(pot)
unless system(gettext, *(targets + ["-o", pot]))
  STDERR.puts("Can't run: #{gettext} #{targets.join(' ')} -o #{pot}")
end

Dir.glob("#{podir}*") do |dir|
  if File.directory?(dir)
    po = "#{dir}/#{appname}.po"
    if File.exist?(po)
      unless system(msgmerge, "-U", po, pot)
        STDERR.puts("Can't run: #{msgmerge} -U #{po} #{pot}")
      end
    else
      FileUtils.cp(pot, po)
    end
  end
end
