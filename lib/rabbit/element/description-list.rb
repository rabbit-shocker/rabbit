# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "container-element"
require_relative "text-block-element"

module Rabbit
  module Element
    class DescriptionList
      include ContainerElement
      include BlockElement

      def to_html(generator)
        "<dl>\n#{super}\n</dl>"
      end
    end

    class DescriptionListItem
      include ContainerElement
      include BlockElement

      attr_reader :term, :content

      def initialize(term, content)
        super()
        @term = term
        @content = content
        add_element(@term)
        add_element(@content)
      end
    end

    class DescriptionTerm
      include TextBlockElement

      def to_rd
        ": #{text}"
      end

      def to_html(generator)
        "<dt>\n#{super}\n</dt>"
      end
    end

    class DescriptionContent
      include ContainerElement
      include BlockElement

      def text
        super.gsub(/^/, "  ")
      end

      def to_rd
        text.gsub(/^/, " ")
      end

      def to_html(generator)
        "<dd>\n#{super}\n</dd>"
      end
    end
  end
end
