require 'rabbit/element/base'
require 'rabbit/element/text-container-element'

module Rabbit
  module Element
    class WaitTag
      include Base

      def have_wait_tag?
        true
      end

      def text
        ""
      end

      def markuped_text
        ""
      end

      def to_html(generator)
        ""
      end

      def draw_element(canvas, x, y, w, h, simulation)
        [x, y, w, h]
      end
    end

    class CustomTag
      include TextContainerElement

      attr_reader :name
      def initialize(name, elements=[])
        super(elements)
        @name = name
      end

      def have_tag?(name)
        @name == name
      end
    end
  end
end
