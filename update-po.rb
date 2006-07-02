#!/usr/bin/env ruby

require "fileutils"

rgettext = "rgettext"
xgettext = "xgettext"
msgmerge = "msgmerge"

po_dir = "po/"
appname = "rabbit"

tmp_dir = "#{po_dir}tmp"
top_dir = "./" + ("../" * tmp_dir.split("/").size)
pot = "#{top_dir}#{po_dir}#{appname}.pot"
current_dir = Dir.pwd

FileUtils.mkdir_p(tmp_dir)
Dir.chdir(tmp_dir)

targets = Dir.glob("#{top_dir}lib/**/*.rb")
targets += Dir.glob("#{top_dir}bin/*")
targets += Dir.glob("#{top_dir}data/**/*.rb")


FileUtils.rm_f(pot)
rgettext_args = targets + ["-o", pot]
unless system(rgettext, *rgettext_args)
  STDERR.puts("Can't run: #{rgettext} #{rgettext_args.join(' ')}")
  exit(1)
end

targets = Dir.glob("#{top_dir}lib/**/*.erb")
xgettext_args = [
  "-L", "PHP", "-k_", "-kN_", "-j", "-o", pot, *targets
]
unless system(xgettext, *xgettext_args)
  STDERR.puts("Can't run: #{xgettext} #{xgettext_args.join(' ')}")
  exit(1)
end

Dir.glob("#{top_dir}#{po_dir}*") do |dir|
  if File.directory?(dir)
    next if File.expand_path(dir) == File.expand_path(tmp_dir)
    po = "#{dir}/#{appname}.po"
    if File.exist?(po)
      args = ["-s", "-U", po, pot]
      unless system(msgmerge, *args)
        STDERR.puts("Can't run: #{msgmerge} #{args.join(' ')}")
      end
    else
      FileUtils.cp(pot, po)
    end
  end
end

Dir.chdir(current_dir)
FileUtils.rm_rf(tmp_dir)
