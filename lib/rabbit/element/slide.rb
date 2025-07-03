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

require_relative "slide-element"
require_relative "text-block-element"

module Rabbit
  module Element
    class Slide
      include SlideElement

      def initialize(title)
        super
      end

      def headline
        @elements[0]
      end

      def body
        @elements[1]
      end

      def to_html(generator)
        "<div class=\"slide\">\n#{super}\n</div>"
      end

      def hide_title?
        self["hide-title"] == "true"
      end
    end

    class HeadLine
      include TextBlockElement

      def to_rd
        "= #{text}"
      end

      def to_html(generator)
        "<h1>#{super}</h1>"
      end

      def draw_elements(canvas, x, y, w, h, simulation)
        return [x, y, w, h] if slide.hide_title?
        super
      end
    end

    class Body
      include ContainerElement
    end
  end
end
