require 'rabbit/element/text-container-element'
require 'rabbit/element/container-element'
require 'rabbit/element/block-element'

module Rabbit
  module Element
    class Container
      include ContainerElement
    end

    class BlockQuote
      include ContainerElement
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
      include TextContainerElement

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

      def have_wait_tag?
        true
      end
    end
  end
end
