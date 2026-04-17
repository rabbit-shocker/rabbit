# Copyright (C) 2006-2026  Sutou Kouhei <kou@cozmixng.org>
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

require "forwardable"

require_relative "../../graffiti/processor"

module Rabbit
  module Renderer
    module Display
      module Graffiti
        extend Forwardable

        def_delegator(:@graffiti, :color, :graffiti_color)
        def_delegator(:@graffiti, :color=, :graffiti_color=)
        def_delegator(:@graffiti, :line_width, :graffiti_line_width)
        def_delegator(:@graffiti, :line_width=, :graffiti_line_width=)
        def_delegator(:@graffiti, :clear_config, :clear_graffiti_config)

        def initialize(*args, &block)
          super
          init_graffiti
        end

        def have_graffiti?
          @graffiti.have_graffiti?
        end

        def can_undo_graffiti?
          @graffiti.can_undo?
        end

        def post_toggle_graffiti_mode
          super
          update_menu if respond_to?(:update_menu)
        end

        def clear_graffiti
          @graffiti.clear
          queue_draw
        end

        def undo_graffiti
          @graffiti.undo
          queue_draw
        end

        def change_graffiti_color
          @graffiti.change_color do
            redraw
          end
        end

        private
        def init_graffiti
          @graffiti = Rabbit::Graffiti::Processor.new

          pressed_button = nil
          target_button = 1

          add_button_press_hook do |event|
            pressed_button = event.button
            if @canvas.graffiti_mode? and event.button == target_button
              @graffiti.button_press(event.x, event.y, width, height)
              true
            else
              false
            end
          end

          add_button_release_hook do |event, last_button_press_event|
            pressed_button = nil
            if @canvas.graffiti_mode? and event.button == target_button
              @graffiti.button_release(event.x, event.y, width, height)
              true
            else
              false
            end
          end

          add_motion_notify_hook do |x, y|
            if @canvas.graffiti_mode? and
                @graffiti.dragging? and
                pressed_button == target_button
              @graffiti.button_motion(x, y, width, height)
              redraw
              true
            else
              false
            end
          end
        end

        def draw_graffiti
          @graffiti.draw_all_segment(self)
        end
      end
    end
  end
end
