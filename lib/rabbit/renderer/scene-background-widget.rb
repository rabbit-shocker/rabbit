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
    class SceneBackgroundWidget < Gtk::Widget
      type_register

      def initialize(canvas, renderer, size)
        super()
        @canvas = canvas
        @renderer = renderer
        @size = size
      end

      def virtual_do_measure(orientation, for_size)
        if orientation == Gtk::Orientation::HORIZONTAL
          width = @size.base_width
          [width, width, -1, -1]
        else
          height = @size.base_height
          [height, height, -1, -1]
        end
      end

      def virtual_do_snapshot(snapshot)
        snapshot.save do
          snapshot.scale(*@size.logical_scale)

          background = @renderer.background.to_gdk_rgba
          snapshot.append_color(background,
                                [
                                  @size.logical_margin_left,
                                  @size.logical_margin_top,
                                  @size.logical_width,
                                  @size.logical_height,
                                ])

          return unless @size.have_logical_margin?

          margin_background = @renderer.make_color("black").to_gdk_rgba
          if @size.have_logical_margin_x?
            snapshot.append_color(margin_background,
                                  [
                                    0,
                                    0,
                                    @size.logical_margin_left,
                                    @size.logical_height,
                                  ])
            snapshot.append_color(margin_background,
                                  [
                                    @size.logical_margin_right +
                                    @size.logical_width,
                                    0,
                                    @size.logical_margin_left,
                                    @size.logical_height,
                                  ])
          end
          if @size.have_logical_margin_y?
            snapshot.append_color(margin_background,
                                  [
                                    0,
                                    0,
                                    @size.logical_width,
                                    @size.logical_margin_top,
                                  ])
            snapshot.append_color(margin_background,
                                  [
                                    0,
                                    @size.logical_margin_top +
                                    @size.logical_height,
                                    @size.logical_width,
                                    @size.logical_margin_bottom,
                                  ])
          end
        end
      end
    end
  end
end
