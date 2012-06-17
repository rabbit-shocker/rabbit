require 'rabbit/element'
require 'rabbit/parser/ext/entity'

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
