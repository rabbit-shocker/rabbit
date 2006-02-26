require 'rabbit/element/text-container-element'
require 'rabbit/element/block-element'

module Rabbit
  module Element
    class PreformattedBlock
      include TextContainerElement
      include BlockHorizontalCentering

      def to_html
        "<pre>#{super}</pre>"
      end
    end

    class PreformattedText
      include TextContainerElement
    end
  end
end
