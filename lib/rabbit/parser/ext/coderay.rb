require 'coderay'

require 'rabbit/utils'
require 'rabbit/parser/ext/escape'

module Rabbit
  module Parser
    module Ext
      module CodeRay
        include GetText

        module_function
        def highlight(lang, text, logger)
          tokens = ::CodeRay.scan(text.strip, lang.to_sym)
          tokens.encode(RabbitEncoder.new)
        end

        class RabbitEncoder < ::CodeRay::Encoders::Encoder
          include Element

          def setup(options)
            super
            @out = SyntaxHighlightingBlock.new
            @elements = [@out]
          end

          def text_token(text, type=:plain)
            # p [:text, type, text]
            escaped_text = Escape.escape_meta_character(text)
            text_element = SyntaxHighlightingText.new(Text.new(escaped_text))
            tag_name = type.to_s.gsub(/_/, '-')
            CustomTag.new("syntax-#{tag_name}", text_element)
          end

          def open_token(kind)
            # p [:open, kind]
            @out = TextContainer.new
            @elements << @out
            CustomTag.new("syntax-#{kind}")
          end

          def begin_line(kind)
            # p [:begin_line, kind]
            nil
          end

          def end_line(kind)
            # p [:end_line, kind]
            nil
          end

          def close_token(kind)
            # p [:close, kind]
            block = @elements.pop
            @out = @elements.last
            block
          end

          def finish(options)
            super
          end
        end
      end
    end
  end
end
