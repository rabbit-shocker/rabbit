# Copyright (C) 2026  Sutou Kouhei <kou@cozmixng.org>
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
    class SceneGraffitiWidget < Gtk::Widget
      type_register

      def initialize(canvas, renderer, size)
        super()
        @canvas = canvas
        @renderer = renderer
        @size = size

        @gesture = Gtk::GestureStylus.new
        @gesture.stylus_only = false
        segment = nil
        @segments = []
        @gesture.signal_connect(:up) do |_, x, y|
          segment = nil
        end
        @gesture.signal_connect(:down) do |_, x, y|
          if @canvas.graffiti_mode?
            segment = [[x, y]]
            @segments << segment
          end
        end
        @gesture.signal_connect(:motion) do |_, x, y|
          if @canvas.graffiti_mode? and segment
            segment << [x, y]
            queue_draw
          end
        end
        add_controller(@gesture)
      end

      def virtual_do_measure(orientation, for_size)
        if orientation == Gtk::Orientation::HORIZONTAL
          minimum = @size.base_width
          natural = [minimum, @size.real_width].max
        else
          minimum = @size.base_height
          natural = [minimum, @size.real_height].max
        end
        # TODO: We want to remove this to accept narrowing a window
        # after full-screen but GTK doesn't use the natural size on
        # the case. If GTK uses the minimum size not the natural size,
        # graffiti might not work because Gtk::GestureStyle detects
        # actions only in the widget size.
        minimum = natural
        [minimum, natural, -1, -1]
      end

      def virtual_do_snapshot(snapshot)
        @renderer.push_snapshot(snapshot, 0, 0) do
          @segments.each do |points|
            @renderer.draw_lines(points,
                                 @renderer.graffiti_color,
                                 line_width: @renderer.graffiti_line_width,
                                 opened: true)
          end

          builder = Gsk::PathBuilder.new
          builder.add_rect([1, 1, width - 2, height - 2])
          stroke = Gsk::Stroke.new(1)
          snapshot.append_stroke(builder.to_path, stroke, Gdk::RGBA.parse("red"))
        end
      end

      def clear
        @segments.clear
        queue_draw
      end
    end
  end
end
