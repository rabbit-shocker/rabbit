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

external_params = {}
entities = {}
ARGF.each do |line|
  case line
  when /^<!ENTITY\s+%\s+(\w+)\s+"(\S+)"\s*>/
    # p ["%", $1, $2]
    external_params[$1] = expand_ref($2)
  when /^<!ENTITY\s+(\w+)\s+"(\S+)"\s*>\s*<!--\s*(.+)\s*-->/
    key = $1
    comment = $3.strip
    value = expand_ext_ref($2.gsub(/&#38;/, '&'), external_params)
    next if entities.key?(key)
    entities[key] = {
      :value   => value,
      :comment => comment,
    }
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

  entities.keys.each do |key|
    attributes = entities[key]
    value   = attributes[:value]
    comment = attributes[:comment]
    out.puts("          # #{comment}") unless comment.empty?
    out.puts("          #{key.dump} => #{value.dump},")
  end

  out.print <<-FOOTER
        }
      end
    end
  end
end
  FOOTER
end
