#!/usr/bin/env ruby

require "rexml/document"

require "rabbit/utils"

include Rabbit::Utils

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

ARGV.each do |name|
  basename = File.basename(name, ".*")
  File.open(name) do |src|
    File.open(File.join(*%W(lib rabbit entity #{basename})) + ".rb", "w") do |out|

      out.print <<-HEADER
require 'rabbit/element'

module Rabbit
  module Entity
    module #{to_class_name(basename)}

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end

HEADER

      ext_param = {}
      src.each do |line|
        case line
        when /^<!ENTITY\s+%\s+(\w+)\s+"(\S+)"\s*>/
          # p ["%", $1, $2]
          ext_param[$1] = expand_ref($2)
        when /^<!ENTITY\s+(\w+)\s+"(\S+)"\s*>\s*<!--\s*(.+)\s*-->/
          out.print <<-METHOD

      # #{$3}
      def ext_inline_verb_#{$1}(label, content, visitor)
        label = label.to_s
        return nil unless /^#{$1}:(.*)$/ =~ label
        NormalText.new(#{expand_ext_ref($2, ext_param).dump})
      end
METHOD
          # p [$1, expand_ext_ref($2, ext_param), $3, name]
        end
      end

      out.print <<-FOOTER

    end
  end
end
FOOTER

    end
  end
end
