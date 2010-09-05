require 'rabbit/element/container-element'

module Rabbit
  module Element
    class EnumList
      include ContainerElement
      include BlockElement

      def to_html(generator)
        "<ol>\n#{super}\n</ol>"
      end
    end

    class EnumListItem
      include ContainerElement
      include BlockElement

      attr_accessor :order

      def to_rd
        prefix = "(#{order}) "
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

