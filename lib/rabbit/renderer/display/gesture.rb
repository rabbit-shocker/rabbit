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

          pressed_info = nil
          target_button = 3
          target_event_type = Gdk::Event::Type::BUTTON_PRESS
          target_info = [target_button, target_event_type]

          add_button_press_hook do |event|
            pressed_info = [event.button, event.event_type]
            if pressed_info == target_info
              x, y, w, h = @area.allocation.to_a
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
          @gesture.draw(@drawable) if gesturing? and @gesture.moved?
        end
      end
    end
  end
end

