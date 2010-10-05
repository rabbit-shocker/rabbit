#!/usr/bin/env ruby

$KCODE = "utf8"

require 'nkf'
require 'rwiki/soap/driver'

def convert(src)
  NKF.nkf("-w", src)
end

def update_source(driver, src, name, page_name, log=nil)
  if src == driver.src(page_name)
    puts "#{name} doesn't need update"
    return
  end
  if log.nil?
    ENV["LANG"] = "C"
    if /Last Changed Rev: (\d+)/ =~ `svn info #{name}`
      log = "update to r#{$1}"
    end
  end
  driver.submit(page_name, src, nil, log)
  puts "committed #{name}"
end

def update_rd(driver, name, page_name=nil, prefix=nil)
  page_name ||= name
  prefix ||= "Rabbit::"
  src = convert(File.read(name))
  src = yield(src, page_name, prefix) if block_given?
  page_name = "#{prefix}#{page_name}"
  update_source(driver, src, name, page_name)
end

def update_index(driver, prev_version, current_version)
  page_name = "Rabbit"
  src = driver.src(page_name)
  src.gsub!(/#{prev_version}/, current_version)
  log = "#{prev_version} -> #{current_version}"
  update_source(driver, src, "index", page_name, log)
end

if ARGV.size < 2
  puts "Usage: #{$0} prev_version current_version"
  exit
end

prev_version, current_version = ARGV

end_point = "http://www.cozmixng.org/~rwiki/rw-soap.rb"
driver = RWiki::SOAP::Driver.new(end_point)

args = [driver, "misc/emacs/README.ja", "README.ja", "rabbit-mode.el::"]
update_rd(*args) do |src, page_name, prefix|
  src.gsub(/\(\(<(.*?)>\)\)/, "((<#{prefix}#{page_name}/\\1>))")
end

[
 ["NEWS.ja"],
 ["NEWS.en"],
 ["README.ja"],
 ["README.en"],
 ["doc/INSTALL.macosx-macports.ja", "INSTALL.macosx-macports.ja"],
 ["doc/INSTALL.macosx-macports.en", "INSTALL.macosx-macports.en"],
 ["doc/INSTALL.macosx-homebrew.ja", "INSTALL.macosx-homebrew.ja"],
 ["doc/INSTALL.macosx-homebrew.en", "INSTALL.macosx-homebrew.en"],
 ["doc/INSTALL.win32.ja", "INSTALL.win32.ja"],
 ["doc/INSTALL.win32.en", "INSTALL.win32.en"],
 ["sample/rabbit.rd", "sample.ja"],
 ["sample/rabbit-en.rd", "sample.en"],
 ["sample/rabbit-implementation.rd", "Implementation.ja"],
 ["sample/can_rabbit.rd", "CanRabbit.ja"],
].each do |name, page_name, prefix|
  update_rd(driver, name, page_name, prefix) do |src, _page_name, _prefix|
    if /\.(ja|en)\z/ =~ _page_name
      lang = $1
      src.gsub(/\(\(<(INSTALL.win32.#{lang})>\)\)/, "((<#{_prefix}\\1>))")
    else
      src
    end
  end
end

update_index(driver, prev_version, current_version)
