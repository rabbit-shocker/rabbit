require 'rabbit/element'
require 'rabbit/parser/ext/entity'

module Rabbit
  module Parser
    class RD
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

          TABLE = Parser::Ext::Entity::TABLE
          def ext_inline_verb_entity_reference(label, source, content, visitor)
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
        end
      end
    end
  end
end
