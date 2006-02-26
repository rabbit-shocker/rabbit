require 'rabbit/element/text-renderer'

module Rabbit
  module Element
    module TextElement
      include TextRenderer

      attr_accessor :text

      def initialize(text)
        super()
        @text = text
      end

      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          draw_layout(canvas, x, y)
        end
        [x + width, y, w - width, h]
      end

      def to_html
        text_to_html
      end

      def empty?
        /\A\s*\z/ =~ @text
      end
    end
  end
end
