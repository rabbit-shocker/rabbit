#!/usr/bin/env ruby

require "fileutils"

rgettext = "rgettext"
xgettext = "xgettext"
msgmerge = "msgmerge"

podir = "po/"
appname = "rabbit"
pot = "#{podir}#{appname}.pot"
targets = Dir.glob("lib/**/*.rb") + Dir.glob("bin/*") + Dir.glob("data/**/*.rb")

FileUtils.rm_f(pot)
rgettext_args = targets + ["-o", pot]
unless system(rgettext, *rgettext_args)
  STDERR.puts("Can't run: #{rgettext} #{rgettext_args.join(' ')}")
  exit(1)
end

targets = Dir.glob("lib/**/*.erb")
xgettext_args = [
  "-L", "PHP", "-k_", "-kN_", "-j", "-o", pot, *targets
]
unless system(xgettext, *xgettext_args)
  STDERR.puts("Can't run: #{xgettext} #{xgettext_args.join(' ')}")
  exit(1)
end

Dir.glob("#{podir}*") do |dir|
  if File.directory?(dir)
    po = "#{dir}/#{appname}.po"
    if File.exist?(po)
      args = ["-U", po, pot]
      unless system(msgmerge, *args)
        STDERR.puts("Can't run: #{msgmerge} #{args.join(' ')}")
      end
    else
      FileUtils.cp(pot, po)
    end
  end
end
