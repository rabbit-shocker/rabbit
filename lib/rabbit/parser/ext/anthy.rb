begin
  require 'anthy'
  Anthy.set_personality("")
rescue LoadError
end

require 'rabbit/utils'

module Rabbit
  module Parser
    module Ext
      module Anthy
        include Element
        include GetText

        module_function
        def available?
          defined?(::Anthy)
        end

        def hiragana_to_kanji(text)
          context = ::Anthy::Context.new
          context.encoding = ::Anthy::EUC_JP_ENCODING
          converted_text = ''
          text.split(/(\s+)/m).each do |sentence|
            if /\A\s+\z/m =~ sentence
              converted_text << sentence
              next
            end
            context.reset
            context.string = Converter.to_eucjp_from_utf8(sentence)
            context.stat.nr_segment.times do |i|
              segment = context.segment(i, 0)
              converted_text << Converter.to_utf8_from_eucjp(segment)
            end
          end

          converted_text
        end
      end
    end
  end
end
