#!/usr/bin/env ruby

if ARGV.size < 1
  puts "#{$0} VERSION [RABBIT] [PS2PDF]"
  exit
end

version, rabbit, ps2pdf, = ARGV
rabbit ||= "bin/rabbit"
ps2pdf ||= "ps2pdf14"
rd_base = "rabbit"

system(rabbit, "-p", "-o", "#{rd_base}_#{version}.ps", "sample/#{rd_base}.rd")

["", "_en"].each do |lang|
  rd = File.join("sample", "#{rd_base}#{lang}.rd")
  ps = "#{rd_base}#{lang}_#{version}.ps"
  index_ps = "#{rd_base}#{lang}_index_#{version}.ps"
  system(rabbit, "-p", "-o", ps, rd)
  system(ps2pdf, ps)
  system(rabbit, "-p", "-o", index_ps, "--slides-per-page", "8", rd)
  system(ps2pdf, index_ps)
end
