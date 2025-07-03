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

require_relative "../../gesture/handler"

module Rabbit
  module Renderer
    module Display
      module Gesture
        def initialize(*args, &block)
          super
          init_gesture
        end

        private
        def init_gesture
          @gesture = Rabbit::Gesture::Handler.new

          pressed_info = nil
          target_button = 3
          target_event_type = Gdk::EventType::BUTTON_PRESS
          target_info = [target_button, target_event_type]

          add_button_press_hook do |event|
            pressed_info = [event.button, event.event_type]
            if pressed_info == target_info and event.state.to_i.zero?
              x, y, w, h = widget.allocation.to_a
              @gesture.start(target_button, x + event.x, y + event.y, x, y)
            end
            false
          end

          add_button_release_hook do |event, last_button_press_event|
            pressed_info = nil
            if @gesture.processing? and event.button == target_button
              restore_cursor(:gesture) if @gesture.moved?
              queue_draw
              moved = @gesture.moved?
              @gesture.button_release(event.x, event.y, width, height)
              moved
            else
              false
            end
          end

          add_motion_notify_hook do |event|
            if @gesture.processing? and pressed_info == target_info
              unless @gesture.moved?
                keep_cursor(:gesture)
                update_cursor(:hand)
              end
              first_move = !@gesture.moved?
              handled = @gesture.button_motion(event.x, event.y, width, height)
              queue_draw if handled or first_move
              init_renderer(@surface)
              @gesture.draw_last_locus(self)
              finish_renderer
              true
            else
              false
            end
          end
        end

        def init_gesture_actions
          @gesture.clear_actions
          bg_proc = Utils.process_pending_events_proc
          add_gesture_action(%w(R), "Next")
          add_gesture_action(%w(R L), "LastSlide")
          add_gesture_action(%w(L), "Previous")
          add_gesture_action(%w(L R), "FirstSlide")
          add_gesture_action(%w(U), "Quit")

          add_gesture_action(%w(D), "ToggleIndexMode")
          add_gesture_action(%w(D U), "ToggleFullscreen")
          add_gesture_action(%w(LR), "ToggleGraffitiMode")

          add_gesture_action(%w(UL), "Redraw")
          add_gesture_action(%w(UL D), "ReloadTheme")
        end

        def add_gesture_action(sequence, action)
          @gesture.add_action(sequence, @canvas.actions.find_action(action))
        end

        def gesturing?
          @gesture.processing?
        end

        def draw_gesture
          @gesture.draw(self) if gesturing? and @gesture.moved?
        end
      end
    end
  end
end

