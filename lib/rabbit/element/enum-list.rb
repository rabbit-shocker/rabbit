require 'rabbit/element/container-element'

module Rabbit
  module Element
    class EnumList
      include ContainerElement

      def to_html(generator)
        "<ol>\n#{super}\n</ol>"
      end
    end

    class EnumListItem
      include ContainerElement
      attr_accessor :order

      def to_html(generator)
        "<li>\n#{super}\n</li>"
      end
    end
  end
end

