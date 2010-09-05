require 'rabbit/element/container-element'
require 'rabbit/element/text-block-element'

module Rabbit
  module Element
    class DescriptionList
      include ContainerElement
      include BlockElement

      def to_html(generator)
        "<dl>\n#{super}\n</dl>"
      end
    end

    class DescriptionListItem
      include ContainerElement
      include BlockElement

      attr_reader :term, :content

      def initialize(term, content)
        super()
        @term = term
        @content = content
        add_element(@term)
        add_element(@content)
      end
    end

    class DescriptionTerm
      include TextBlockElement

      def to_rd
        ": #{text}"
      end

      def to_html(generator)
        "<dt>\n#{super}\n</dt>"
      end
    end

    class DescriptionContent
      include ContainerElement
      include BlockElement

      def text
        super.gsub(/^/, "  ")
      end

      def to_rd
        text.gsub(/^/, " ")
      end

      def to_html(generator)
        "<dd>\n#{super}\n</dd>"
      end
    end
  end
end
