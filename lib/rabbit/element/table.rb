require 'rabbit/element/text-element'
require 'rabbit/element/container-element'

module Rabbit
  module Element
    class Table
      include ContainerElement

      attr_reader :caption
      def initialize(prop)
        super()
        %w(caption).each do |name|
          instance_variable_set("@#{name}", prop[name])
        end
      end

      def head
        elements.find {|e| e.is_a?(TableHead)}
      end

      def body
        elements.find {|e| e.is_a?(TableBody)}
      end

      def to_html
        caption = nil
        caption = "<caption>#{@caption}</caption>\n" if @caption
        "<table>\n#{caption}#{super}\n</table>"
      end
    end

    class TableHead
      include ContainerElement

      def to_html
        "<thead>\n#{super}\n</thead>"
      end
    end

    class TableBody
      include ContainerElement

      def to_html
        "<tbody>\n#{super}\n</tbody>"
      end
    end

    class TableRow
      include ContainerElement

      def to_html
        "<tr>\n#{super}\n</tr>"
      end
    end

    class TableHeader
      include TextElement

      def to_html
        "<th>#{super}</th>"
      end
    end

    class TableCell
      include TextElement

      def to_html
        "<td>#{super}</td>"
      end
    end
  end
end
