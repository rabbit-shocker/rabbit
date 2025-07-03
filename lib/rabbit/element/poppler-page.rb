# Copyright (C) 2007-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "block-element"

module Rabbit
  module Element
    class PopplerPage
      include Base
      include BlockElement
      include BlockHorizontalCentering

      def initialize(page)
        @page = page
        super()
      end

      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          canvas.draw_poppler_page(@page, x, y, :width => w, :height => h)
        end
        [x, y + height, w, h - height]
      end

      def title
        text.split(/\r?\n/, 2).first
      end

      def text
        @page.text
      end

      def clear_theme
        super
        @width, @height = @page.size
      end
    end
  end
end
