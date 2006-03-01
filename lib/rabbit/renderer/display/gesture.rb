require "rabbit/gesture/handler"

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

          pressed_button = nil
          first_motion = false
          target_button = 3

          add_button_press_hook do |event|
            pressed_button = event.button
            first_motion = true
            if event.button == target_button
              x, y, w, h = @area.allocation.to_a
              @gesture.start(target_button, x + event.x, y + event.y, x, y)
            end
            false
          end

          add_button_release_hook do |event, last_button_press_event|
            pressed_button = nil
            if @gesture.processing? and event.button == target_button
              restore_cursor(:gesture) unless first_motion
              queue_draw
              @gesture.button_release(event.x, event.y, width, height)
              !first_motion
            else
              false
            end
          end

          add_motion_notify_hook do |event|
            if @gesture.processing? and pressed_button == target_button
              if first_motion
                keep_cursor(:gesture)
                update_cursor(:hand)
              end
              handled = @gesture.button_motion(event.x, event.y, width, height)
              queue_draw if handled or first_motion
              first_motion = false
              @gesture.draw_last_locus(@drawable)
              true
            else
              false
            end
          end
        end

        def init_gesture_actions
          @gesture.clear_actions
          bg_proc = Utils.process_pending_events_proc
          add_gesture_action(%w(R), "NextSlide")
          add_gesture_action(%w(R L), "LastSlide")
          add_gesture_action(%w(L), "PreviousSlide")
          add_gesture_action(%w(L R), "FirstSlide")
          add_gesture_action(%w(U), "Quit")

          add_gesture_action(%w(D), "ToggleIndexMode", &bg_proc)
          add_gesture_action(%w(D U), "ToggleFullScreen", &bg_proc)
          add_gesture_action(%w(LR), "ToggleGraffitiMode")

          add_gesture_action(%w(UL), "Redraw")
          add_gesture_action(%w(UL D), "ReloadTheme", &bg_proc)
        end

        def add_gesture_action(sequence, action, &block)
          @gesture.add_action(sequence, @canvas.action(action), &block)
        end

        def gesturing?
          @gesture.processing?
        end

        def draw_gesture
          @gesture.draw(@drawable) if gesturing?
        end
      end
    end
  end
end

