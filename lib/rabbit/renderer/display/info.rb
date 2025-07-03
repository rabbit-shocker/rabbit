# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../../info-window"

module Rabbit
  module Renderer
    module Display
      module Info
        def initialize(canvas)
          super
          @info_window = InfoWindow.new(@canvas)
        end

        def toggle_info_window
          if info_window_showing?
            hide_info_window
          else
            show_info_window
          end
        end

        def info_window_showing?
          @info_window.showing?
        end

        def show_info_window
          @info_window.show(width, height)
        end

        def hide_info_window
          @info_window.hide
        end

        def post_parse
          super
          @info_window.parsed
        end

        def post_move(old_index, index)
          super
          @info_window.moved
        end

        def post_move_in_slide(old_index, index)
          super
          @info_window.moved
        end

        def index_mode_on
          super
          @info_window.index_mode_on
        end

        def index_mode_off
          super
          @info_window.index_mode_off
        end
      end
    end
  end
end
