require 'rabbit/element/container-element'
require 'rabbit/element/text-container-element'

module Rabbit
  module Element
    class DescriptionList
      include ContainerElement

      def to_html(generator)
        "<dl>\n#{super}\n</dl>"
      end
    end

    class DescriptionListItem
      include ContainerElement

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
      include TextContainerElement

      def to_html(generator)
        "<dt>\n#{super}\n</dt>"
      end
    end

    class DescriptionContent
      include ContainerElement

      def to_html(generator)
        "<dd>\n#{super}\n</dd>"
      end
    end
  end
end
