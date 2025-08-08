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

module Rabbit
  module Renderer
    module Display
      module Spotlight
        def initialize(canvas)
          super
          init_spotlight
        end

        def toggle_spotlight
          @spotlighting = !@spotlighting
          if spotlighting?
            grab
            @spotlight_center_x ||= 0
            @spotlight_center_y ||= 0
          else
            ungrab
            @spotlight_center_x = nil
            @spotlight_center_y = nil
          end
          queue_draw
        end

        def spotlighting?
          @spotlighting
        end

        private
        def init_spotlight
          @spotlighting = false

          @spotlight_radius_ratio = 0.1
          @spotlight_center_x = nil
          @spotlight_center_y = nil

          add_button_press_hook do |event|
            is_target = lambda do
              return false unless event.button == 3
              if Gdk::EventType.const_defined?(:BUTTON2_PRESS)
                event.event_type == Gdk::EventType::BUTTON2_PRESS
              else
                event.event_type == Gdk::EventType::BUTTON_PRESS and
                  event.n_presses == 2
              end
            end
            if is_target.call
              add_button_handler do
                @spotlight_center_x = event.x
                @spotlight_center_y = event.y
                @canvas.activate("ToggleSpotlight")
                clear_button_handler
                true
              end
            end
            false
          end

          add_motion_notify_hook do |x, y|
            if spotlighting?
              @spotlight_center_x = x
              @spotlight_center_y = y
              queue_draw
              true
            else
              false
            end
          end

          add_scroll_hook do |event|
            if spotlighting?
              case event.direction
              when Gdk::ScrollDirection::UP
                @spotlight_radius_ratio =
                  [0.1, @spotlight_radius_ratio - 0.1].max
              when Gdk::ScrollDirection::DOWN
                @spotlight_radius_ratio =
                  [1, @spotlight_radius_ratio + 0.1].min
              end
              queue_draw
              true
            else
              false
            end
          end
        end

        def draw_spotlight
          return unless spotlighting?
          radius = width * @spotlight_radius_ratio
          base = [
                  @spotlight_center_x - radius / 8,
                  @spotlight_center_y,
                  radius / 6,
                  @spotlight_center_x + radius / 8,
                  @spotlight_center_y,
                  radius,
                 ]
          color_stops = [
                         [0, 1, 1, 1, 0],
                         [0.7, 0, 0, 0, 0.8],
                         [1, 0, 0, 0, 0.8],
                        ]
          params = {
            :pattern => {
              :type => :radial,
              :base => base,
              :color_stops => color_stops,
            }
          }
          draw_rectangle(true, 0, 0, size.real_width, size.real_height, nil, params)
        end
      end
    end
  end
end
