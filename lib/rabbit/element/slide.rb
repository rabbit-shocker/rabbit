require 'rabbit/element/slide-element'
require 'rabbit/element/text-container-element'

module Rabbit
  module Element
    class Slide
      include SlideElement

      def headline
        @elements[0]
      end

      def body
        @elements[1]
      end
    end

    class HeadLine
      include TextContainerElement

      def to_html
        "<h1>#{super}</h1>"
      end
    end

    class Body
      include ContainerElement
    end
  end
end
