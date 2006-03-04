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

      def to_rd
        prefix = "* "
        indent = " " * prefix.length
        first, *rest = text.split(/\n/)
        rest = rest.collect do |line|
          "#{indent}#{line}"
        end.join("\n")
        "#{prefix}#{first}\n#{rest}".rstrip
      end

      def to_html(generator)
        "<li>\n#{super}\n</li>"
      end
    end
  end
end
