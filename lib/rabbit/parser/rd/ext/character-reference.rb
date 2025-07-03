# Copyright (C) 2012-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require_relative "../../../element"
require_relative "../../ext/entity"

module Rabbit
  module Parser
    class RD
      module Ext
        module CharacterReference
          include Element

          class << self
            def included(mod)
              self.instance_methods.each do |meth|
                mod.method_added(meth)
              end
            end
          end

          TABLE = Parser::Ext::Entity::TABLE
          def ext_inline_verb_character_entity_reference(label, source, content, visitor)
            label = label.to_s
            return nil unless /^&([^;]+);(.*)$/ =~ label
            return nil unless TABLE.include?($1)

            key = $1
            rest = $2
            if rest.empty?
              Text.new(TABLE[key])
            else
              rest = visitor.apply_to_Verb(::RD::Verb.new(rest))
              TextContainer.new([Text.new(TABLE[key]), rest])
            end
          end

          def ext_inline_verb_numeric_character_reference(label, source, content, visitor)
            label = label.to_s
            return nil unless /^&#(x?)([a-zA-Z\d]+);(.*)$/ =~ label

            base = $1
            unicode_code_point_string = $2
            rest = $3
            if base == "x"
              unicode_code_point = unicode_code_point_string.to_i(16)
            else
              unicode_code_point = unicode_code_point_string.to_i(10)
            end
            character = [unicode_code_point].pack("U")
            if rest.empty?
              Text.new(character)
            else
              rest = visitor.apply_to_Verb(::RD::Verb.new(rest))
              TextContainer.new([Text.new(character), rest])
            end
          end
        end
      end
    end
  end
end
