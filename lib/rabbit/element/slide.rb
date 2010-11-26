require 'rabbit/element/slide-element'
require 'rabbit/element/text-block-element'

module Rabbit
  module Element
    class Slide
      include SlideElement

      def initialize(title)
        super
      end

      def headline
        @elements[0]
      end

      def body
        @elements[1]
      end

      def to_html(generator)
        "<div class=\"slide\">\n#{super}\n</div>"
      end

      def hide_title?
        self["hide-title"] == "true"
      end
    end

    class HeadLine
      include TextBlockElement

      def to_rd
        "= #{text}"
      end

      def to_html(generator)
        "<h1>#{super}</h1>"
      end

      def draw_elements(canvas, x, y, w, h, simulation)
        return [x, y, w, h] if slide.hide_title?
        super
      end
    end

    class Body
      include ContainerElement
    end
  end
end
