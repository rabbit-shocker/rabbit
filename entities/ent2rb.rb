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

File.open(File.join(*%W(lib rabbit ext entity)) + ".rb", "w") do |out|
  out.print <<-HEADER
require 'rabbit/element'

module Rabbit
  module Ext
    module Entity

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end

      def ext_inline_verb_entity_reference(label, content, visitor)
        label = label.to_s
        return nil unless /^&([^;]+);(.*)$/ =~ label
        return nil unless TABLE.include?($1)

        key = $1
        rest = $2
        if rest.empty?
          NormalText.new(TABLE[key])
        else
          rest = visitor.apply_to_Verb(RD::Verb.new(rest))
          TextContainer.new([NormalText.new(TABLE[key]), rest])
        end
      end

      TABLE = {
HEADER

      ext_param = {}
      ARGF.each do |line|
        case line
        when /^<!ENTITY\s+%\s+(\w+)\s+"(\S+)"\s*>/
          # p ["%", $1, $2]
          ext_param[$1] = expand_ref($2)
        when /^<!ENTITY\s+(\w+)\s+"(\S+)"\s*>\s*<!--\s*(.+)\s*-->/
          out.print <<-METHOD
        # #{$3}
        #{$1.dump} => #{expand_ext_ref($2, ext_param).dump},
METHOD
          # p [$1, expand_ext_ref($2, ext_param), $3, name]
        end
      end

      out.print <<-FOOTER
      }

    end
  end
end
FOOTER

end
