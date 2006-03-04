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

      def text
        prefix = "(#{order}) "
        indent = " " * prefix.length
        first, *rest = super.split(/\n/)
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

