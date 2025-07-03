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

require_relative "base"

module Rabbit
  module Element
    module BlockHorizontalCentering
      attr_reader :ox, :oy, :ow, :oh # dirty!!!!

      def do_horizontal_centering(canvas, x, y, w, h)
        @ox, @oy, @ow, @oh = @x, @y, @w, @h
        adjust_width = ((w - width) / 2.0).ceil
        x += adjust_width
        w -= adjust_width
        @centering_adjusted_width = adjust_width
        compile_for_horizontal_centering(canvas, x, @y, w, h)
        draw(true)
      end

      def reset_horizontal_centering(canvas, x, y, w, h)
        # TODO
      end

      def clear_theme
        @ox = @oy = @ow = @oh = nil
        super
      end
    end

    module BlockElement
      def inline_element?
        false
      end

      def adjust_y_padding(y, h)
        y += @padding_bottom
        h -= @padding_bottom
        [y, h]
      end
    end
  end
end
