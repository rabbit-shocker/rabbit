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

require_relative "../rabbit"

module Rabbit
  module Renderer
    class PrintLayout
      class << self
        def create(renderer, canvas)
          map = MAPPING.find do |key, value|
            key === renderer.slides_per_page
          end
          if map.nil?
            raise InvalidPages.new(renderer.slides_per_page)
          else
            map[1].new(renderer, canvas)
          end
        end
      end

      def initialize(renderer, canvas)
        @renderer = renderer
        @canvas = canvas
        @margin_left = renderer.margin_left
        @margin_right = renderer.margin_right
        @margin_top = renderer.margin_top
        @margin_bottom = renderer.margin_bottom
      end
    end

    class PrintLayout1 < PrintLayout
      def slide_width
        @renderer.page_width
      end

      def slide_height
        @renderer.page_height
      end

      def normalize_x(x)
        x
      end

      def normalize_y(y)
        y
      end
    end

    class PrintLayout2 < PrintLayout
      def initialize(renderer, canvas)
        super
        @margin_left ||= 50
        @margin_right ||= 50
        @margin_top ||= 30
        @margin_bottom ||= 30
      end

      def slide_width
        @renderer.page_width - @margin_left - @margin_right
      end

      def slide_height
        (@renderer.page_height / 2) - @margin_top - @margin_bottom
      end

      def normalize_x(x)
        x + @margin_left
      end

      def normalize_y(y)
        base = @margin_bottom
        if below?
          base += @margin_top + @margin_bottom + slide_height
        end
        y + base
      end

      private
      def below?
        (@canvas.current_index % 2) == 1
      end
    end

    class PrintLayoutMore < PrintLayout
      def initialize(renderer, canvas)
        super
        @margin_left ||= 10
        @margin_right ||= 10
        @margin_top ||= 10
        @margin_bottom ||= 10
      end

      def slide_width
        (@renderer.page_width / 2) - @margin_left - @margin_right
      end

      def slide_height
        base = @renderer.page_height / (@renderer.slides_per_page / 2.0).ceil
        base - @margin_top - @margin_bottom
      end

      def normalize_x(x)
        base = @margin_left
        unless left?
          base += @margin_left + @margin_right + slide_width
        end
        x + base
      end

      def normalize_y(y)
        nth_col = (normalized_current_index / 2.0).truncate
        base = @margin_top * (nth_col + 1)
        base += @margin_bottom * nth_col
        base += slide_height * nth_col
        y + base
      end

      private
      def left?
        (normalized_current_index % 2).zero?
      end

      def normalized_current_index
        @canvas.current_index % @renderer.slides_per_page
      end
    end

    class PrintLayout
      MAPPING = [
        [1, PrintLayout1],
        [2, PrintLayout2],
        [3..(1.0/0), PrintLayoutMore],
      ]
    end
  end
end
