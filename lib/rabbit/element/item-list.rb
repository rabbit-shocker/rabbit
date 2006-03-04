require 'rabbit/element/container-element'

module Rabbit
  module Element
    class ItemList
      include ContainerElement

      def to_html(generator)
        "<ul>\n#{super}\n</ul>"
      end
    end

    class ItemListItem
      include ContainerElement

      def text
        "* #{super}"
      end

      def to_html(generator)
        "<li>\n#{super}\n</li>"
      end
    end
  end
end
