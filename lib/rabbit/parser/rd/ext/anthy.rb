require 'rabbit/parser/ext/anthy'

module Rabbit
  module Parser
    class RD
      module Ext
        module Anthy
          include Element
          include GetText

          def anthy_hiragana_to_kanji(label, source, content, visitor)
            unless Parser::Ext::Anthy.available?
              visitor.logger.warning(_("Anthy isn't available"))
              return nil
            end
            src, prop = parse_source(source)

            converted_src = Parser::Ext::Anthy.hiragana_to_kanji(src)
            tree = ::RD::RDTree.new("=begin\n#{converted_src}\n=end\n")
            elems = tree.root.children.collect do |child|
              child.accept(visitor)
            end
            Container.new(elems)
          end
        end
      end
    end
  end
end
