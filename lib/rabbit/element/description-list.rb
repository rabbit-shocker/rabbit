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

      METADATA_LIST = {
        "rabbit-skip-print" => -> (slide){ slide.skip_print }
      }

      def initialize(term, content)
        super()
        @term = term
        @content = content
        add_element(@term)
        add_element(@content)
      end

      def metanize_if_metadata
        term_text = term.map(&:text).join.strip
        if METADATA_LIST.key?(term_text)
          METADATA_LIST[term_text].call(slide)
          parent.delete(self)
        end
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
