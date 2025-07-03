# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require_relative "text-container-element"
require_relative "container-element"
require_relative "block-element"

module Rabbit
  module Element
    class Table
      include ContainerElement
      include BlockElement

      attr_reader :caption
      def initialize(prop={})
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

      alias container_text text
      def text
        result = ''
        result << "#{@caption}\n" if @caption
        result << super
        result
      end

      def to_rd
        result = "# RT\n"
        result << "caption = #{@caption}\n" if @caption
        result << "\n"
        result << container_text
        result.gsub(/^/, "  ")
      end

      def to_html(generator)
        caption = nil
        caption = "<caption>#{@caption}</caption>\n" if @caption
        "<table>\n#{caption}#{super}\n</table>"
      end
    end

    class TableHead
      include ContainerElement
      include BlockElement

      def text
        "#{super}\n"
      end

      def to_html(generator)
        "<thead>\n#{super}\n</thead>"
      end
    end

    class TableBody
      include ContainerElement
      include BlockElement

      def to_html(generator)
        "<tbody>\n#{super}\n</tbody>"
      end
    end

    class TableRow
      include ContainerElement
      include BlockElement

      def text
        super.gsub(/\n/, ", ")
      end

      def to_rd
        text
      end

      def to_html(generator)
        "<tr>\n#{super}\n</tr>"
      end
    end

    class TableHeader
      include TextContainerElement

      def to_html(generator)
        "<th>#{super}</th>"
      end
    end

    class TableCell
      include TextContainerElement

      def to_html(generator)
        "<td>#{super}</td>"
      end
    end
  end
end
