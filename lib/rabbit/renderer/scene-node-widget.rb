# Copyright (C) 2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../gtk"

module Rabbit
  module Renderer
    class SceneNodeWidget < Gtk::Widget
      type_register

      attr_reader :x
      attr_reader :y
      def initialize(canvas, element, x, y, width, height)
        super()
        @canvas = canvas
        @size = @canvas.renderer.size
        @element = element
        @x = x
        @y = y
        @width = width
        @height = height
      end

      def virtual_do_measure(orientation, for_size)
        if orientation == Gtk::Orientation::VERTICAL
          [@height, @height, -1, -1]
        else
          [@width, @width, -1, -1]
        end
      end

      def virtual_do_snapshot(snapshot)
        # For backward compatibility. Legacy DrawingArea based
        # renderer. Legacy DrawingAare based renderer uses uses {x: 0,
        # y: 0, width: @canvas.width, height: @canvas.height} coordinate
        # for all elements. Scene based renderer uses {x: element.x, y:
        # element.y, width: element.width, height: element.height} for
        # each element.
        @canvas.renderer.push_snapshot(snapshot, @x, @y) do
          snapshot.scale(*@size.logical_scale)
          snapshot.translate([
                               @size.logical_margin_left,
                               @size.logical_margin_top,
                             ])
          @element.scene_snapshot(self, snapshot, @canvas, @width, @height)
        end
      end
    end
  end
end
