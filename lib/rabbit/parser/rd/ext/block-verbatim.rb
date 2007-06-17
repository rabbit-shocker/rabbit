require "tempfile"

require 'rabbit/rabbit'
require 'rabbit/utils'
require 'rabbit/tgif'
require 'rabbit/parser/rd/rt/rt2rabbit-lib'
require 'rabbit/parser/rd/ext/base'
require 'rabbit/parser/rd/ext/image'
require 'rabbit/parser/rd/ext/enscript'
require 'rabbit/parser/rd/ext/tex'
require 'rabbit/parser/rd/ext/anthy'

module Rabbit
  module Parser
    class RD
      module Ext
        class BlockVerbatim < Base
          include Image
          include Enscript
          include TeX
          include Anthy
          include GetText

          def default_ext_block_verbatim(label, source, content, visitor)
            content = visitor.apply_to_String(content.rstrip)
            text = Text.new(content)
            PreformattedBlock.new(PreformattedText.new(text))
          end

          def ext_block_verb_quote(label, source, content, visitor)
            return nil unless /^_$/i =~ label
            default_ext_block_verbatim("", source, source, visitor)
          end

          def ext_block_verb_img(label, source, content, visitor)
            return nil unless /^(?:image|img)$/i =~ label
            src, prop = parse_source(source)
            return nil if prop['src'].nil?
            make_image(visitor, prop['src'], prop)
          end

          def ext_block_verb_enscript(label, source, content, visitor)
            return nil unless /^enscript (\w+)$/i =~ label
            lang = $1.downcase.untaint
            enscript_block(label, lang, source, content, visitor)
          end

          def ext_block_verb_anthy(label, source, content, visitor)
            return nil unless /^anthy$/i =~ label
            anthy_hiragana_to_kanji(label, source, content, visitor)
          end

          def ext_block_verb_LaTeX(label, source, content, visitor)
            return nil unless /^LaTeX$/i =~ label
            make_image_by_LaTeX(source, visitor)
          end

          def ext_block_verb_mimeTeX(label, source, content, visitor)
            return nil unless /^mimeTeX$/i =~ label
            make_image_by_mimeTeX(source, visitor)
          end

          def ext_block_verb_Tgif(label, source, content, visitor)
            return nil unless /^Tgif$/i =~ label
            make_image_by_Tgif(source, visitor)
          end

          def ext_block_verb_rt(label, source, content, visitor)
            return nil unless /^rt$/i =~ label
            rt_visitor = RT2RabbitVisitor.new(visitor)
            rt_visitor.visit(RT::RTParser.parse(content))
          end

          def ext_block_verb_block_quote(label, source, content, visitor)
            return nil unless /^blockquote$/i =~ label
            src, prop = parse_source(source)
            tree = ::RD::RDTree.new("=begin\n#{src}\n=end\n")
            elems = tree.root.children.collect do |child|
              child.accept(visitor)
            end
            BlockQuote.new(elems, prop)
          end
        end
      end
    end
  end
end
