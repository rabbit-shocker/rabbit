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

    class PreformattedBlock
      include TextBlockElement

      def to_html(generator)
        "<pre>#{super}</pre>"
      end
    end

    class PreformattedText
      include TextContainerElement
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
    end

    class Subscript
      include TextContainerElement
    end

    class Superscript
      include TextContainerElement
    end
  end
end
