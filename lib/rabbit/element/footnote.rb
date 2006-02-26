require 'rabbit/element/text-element'
require 'rabbit/element/text-container-element'

module Rabbit
  module Element
    class FootTextBlock
      include ContainerElement
    end

    class FootNote
      include TextElement

      attr_reader :order

      def initialize(order)
        @order = order
        super("")
      end
    end

    class FootText
      include TextContainerElement

      attr_reader :order

      def initialize(order, elems=[])
        @order = order
        super(elems)
      end
    end
  end
end
