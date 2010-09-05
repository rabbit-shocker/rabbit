require 'rabbit/element/text-block-element'

module Rabbit
  module Element
    class Container
      include ContainerElement
      include BlockElement
    end

    class BlockQuote
      include ContainerElement
      include BlockElement
      include BlockHorizontalCentering

      attr_reader :cite, :title

      def initialize(elems=[], prop={})
        super(elems)
        %w(cite title).each do |name|
          instance_variable_set("@#{name}", prop[name])
        end
      end

      def to_html(generator)
        "<blockquote>#{super}</blockquote>"
      end
    end

    class Paragraph
      include TextBlockElement

      def text
        "#{super}\n"
      end

      def to_rd
        text
      end

      def to_html(generator)
        "<p>\n#{super}\n</p>"
      end
    end

    class WaitBlock
      include ContainerElement
      include BlockElement

      def have_wait_tag?
        true
      end
    end
  end
end
