require 'rabbit/element/container-element'

module Rabbit
  module Element
    class ItemList
      include ContainerElement

      def to_html
        "<ul>\n#{super}\n</ul>"
      end
    end

    class ItemListItem
      include ContainerElement

      def to_html
        "<li>\n#{super}\n</li>"
      end
    end
  end
end
