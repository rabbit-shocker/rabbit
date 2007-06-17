#!/usr/bin/env ruby

require "rexml/document"

def expand_ref(str)
  REXML::Text.unnormalize(str)
end

def expand_ext_ref(str, table)
  str.gsub(/%([^\s;]+);/) do |x|
    if table.has_key?($1)
      expand_ref(table[$1])
    else
      p table
      raise $1
    end
  end
end

File.open(File.join(*%W(lib rabbit parser ext entity)) + ".rb", "w") do |out|
  out.print <<-HEADER
require 'rabbit/element'

module Rabbit
  module Parser
    module Ext
      module Entity
        TABLE = {
HEADER

      ext_param = {}
      ARGF.each do |line|
        case line
        when /^<!ENTITY\s+%\s+(\w+)\s+"(\S+)"\s*>/
          # p ["%", $1, $2]
          ext_param[$1] = expand_ref($2)
        when /^<!ENTITY\s+(\w+)\s+"(\S+)"\s*>\s*<!--\s*(.+)\s*-->/
          key = $1
          comment = $3.strip
          value = expand_ext_ref($2.gsub(/&#38;/, '&'), ext_param)
          out.print <<-ITEM
          # #{comment}
          #{key.dump} => #{value.dump},
ITEM
          # p [key, value, comment, name]
        end
      end

      out.print <<-FOOTER
        }
      end
    end
  end
end
FOOTER

end
