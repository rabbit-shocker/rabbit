require 'rabbit/element/text-block-element'
require 'rabbit/element/block-element'

module Rabbit
  module Element
    class SyntaxHighlightingBlock
      include TextBlockElement
      include BlockHorizontalCentering

      def text
        super.gsub(/^/, "  ")
      end

      def to_rd
        text
      end

      def to_html(generator)
        "<pre class=\"syntax-highlighting\">#{super}</pre>"
      end
    end

    class SyntaxHighlightingText
      include TextContainerElement
    end
  end
end
