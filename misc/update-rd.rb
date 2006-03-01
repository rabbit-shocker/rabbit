#!/usr/bin/env ruby

$KCODE = "utf8"

require 'nkf'
require 'rwiki/soap/driver'

def convert(src)
  NKF.nkf("-w", src)
end

def commit(driver, name, page_name=name)
  src = convert(File.read(name))
  src = yield(src) if block_given?
  page_name = "Rabbit::#{page_name}"
  if src == driver.src(page_name)
    puts "#{name} doesn't need update"
    return
  end
  ENV["LANG"] = "C"
  log = nil
  if /Last Changed Rev: (\d+)/ =~ `svn info #{name}`
    log = "update to r#{$1}"
  end
  driver.submit(page_name, src, nil, log)
  puts "committed #{name}"
end

end_point = "http://www.cozmixng.org/~rwiki/rw-soap.rb"
driver = RWiki::SOAP::Driver.new(end_point)

%w(ja en).each do |lang|
  %w(README INSTALL.win32).each do |target|
    commit(driver, "#{target}.#{lang}") do |src|
      src.gsub(/\(\(<(INSTALL.win32.#{lang})>\)\)/,
               '((<Rabbit::\1>))')
    end
  end
end

[
 ["sample/rabbit.rd", "sample.ja"],
 ["sample/rabbit_en.rd", "sample.en"],
 ["sample/rabbit-implementation.rd", "Implementation.ja"],
 ["sample/can_rabbit.rd", "CanRabbit.ja"],
].each do |name, page_name|
  commit(driver, name, page_name)
end
