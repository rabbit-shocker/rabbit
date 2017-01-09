require 'rabbit/element/text-element'
require 'rabbit/element/text-container-element'
require 'rabbit/element/text-block-element'

module Rabbit
  module Element
    class Text
      include TextElement
    end

    class TextContainer
      include TextContainerElement
    end

    class TextBlock
      include TextBlockElement
    end

    class Keyword
      include TextContainerElement
    end

    class Comment
      include TextContainerElement
    end

    class Emphasis
      include TextContainerElement
    end

    class Code
      include TextContainerElement
    end

    class Variable
      include TextContainerElement
    end

    class Keyboard
      include TextContainerElement
    end

    class Index
      include TextContainerElement
    end

    class Note
      include TextContainerElement
    end

    class Verbatim
      include TextContainerElement
    end

    class DeletedText
      include TextContainerElement
    end

    class ReferText
      include TextContainerElement

      attr_accessor :to

      # TODO: This makes paragraph instead of word link.
      def draw_sub_elements(canvas, x, y, w, h)
        canvas.draw_link(to) do
          super
        end
      end
    end

    class Subscript
      include TextContainerElement
    end

    class Superscript
      include TextContainerElement
    end
  end
end
