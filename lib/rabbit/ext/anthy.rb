begin
  require 'anthy'
  Anthy.set_personality("")
rescue LoadError
end

require 'rabbit/utils'
require 'rabbit/ext/block-verbatim'

module Rabbit
  module Ext
    module Anthy
      include Element
      include GetText

      def anthy_hiragana_to_kanji(label, source, content, visitor)
        unless defined?(::Anthy)
          visitor.logger.warn(_("Anthy isn't available"))
          return nil
        end
        src, prop = parse_source(source)

        context = ::Anthy::Context.new
        context.encoding = ::Anthy::EUC_JP_ENCODING
        converted_src = ''
        src.split(/(\s+)/m).each do |sentence|
          if /\A\s+\z/m =~ sentence
            converted_src << sentence
            next
          end
          context.reset
          context.string = Converter.to_eucjp_from_utf8(sentence)
          context.stat.nr_segment.times do |i|
            converted_src << Converter.to_utf8_from_eucjp(context.segment(i, 0))
          end
        end

        tree = RD::RDTree.new("=begin\n#{converted_src}\n=end\n")
        elems = tree.root.children.collect do |child|
          child.accept(visitor)
        end
        Container.new(elems)
      end
    end
  end
end
