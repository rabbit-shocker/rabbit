require 'rabbit/element/text-block-element'
require 'rabbit/element/block-element'

module Rabbit
  module Element
    class PreformattedBlock
      include TextBlockElement
      include BlockHorizontalCentering

      def text
        super.gsub(/^/, "  ")
      end

      def to_rd
        text
      end

      def to_html(generator)
        "<pre#{attributes}>#{super}</pre>"
      end

      private
      def attributes
        ""
      end
    end

    class PreformattedText
      include TextContainerElement
    end
  end
end
