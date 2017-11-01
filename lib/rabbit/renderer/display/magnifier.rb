module Rabbit
  module Renderer
    module Display
      module Magnifier
        def initialize(canvas)
          super
          init_magnifier
        end

        def attach_to(window, container=nil)
          super
          magnifier_action.active = false
        end

        def toggle_magnifier
          if magnifying?
            grab
            x, y, mask = pointer
            @magnifier_center_x ||= x
            @magnifier_center_y ||= y
          else
            ungrab
            @magnifier_center_x = nil
            @magnifier_center_y = nil
          end
          queue_draw
        end

        private
        def init_magnifier
          @magnifier_ratio = 1.5
          @magnifier_center_x = nil
          @magnifier_center_y = nil

          target_button = 3
          target_event_type = Gdk::EventType::BUTTON_PRESS
          target_info = [target_button, target_event_type]

          add_button_press_hook do |event|
            if [event.button, event.event_type] == target_info and
                event.state.control_mask?
              @magnifier_center_x = event.x
              @magnifier_center_y = event.y
              magnifier_action.active = true
              true
            else
              false
            end
          end

          add_button_release_hook do |event, last_event|
            if magnifying? and event.button == target_button
              magnifier_action.active = false
              true
            else
              false
            end
          end

          add_motion_notify_hook do |event|
            if magnifying?
              @magnifier_center_x = event.x
              @magnifier_center_y = event.y
              queue_draw
              add_button_handler do
                clear_button_handler
                true
              end
              true
            else
              false
            end
          end

          add_scroll_hook do |event|
            if magnifying?
              case event.direction
              when Gdk::ScrollDirection::UP
                @magnifier_ratio = [0.5, @magnifier_ratio - 0.1].max
              when Gdk::ScrollDirection::DOWN
                @magnifier_ratio = [3, @magnifier_ratio + 0.1].min
              end
              queue_draw
              true
            else
              false
            end
          end
        end

        def magnify(&block)
          return unless magnifying?

          w = width / 1.5
          h = height / 1.5
          center_x = @magnifier_center_x - size.logical_margin_left
          center_y = @magnifier_center_y - size.logical_margin_top
          x = center_x - w / 2
          y = center_y - h / 2
          r = w * 0.1
          save_context do
            clip_block = Proc.new do
              draw_rectangle(true, 0, 0, width, height, @background)
              translate_context(center_x, center_y)
              scale_context(@magnifier_ratio, @magnifier_ratio)
              translate_context(-center_x, -center_y)
              block.call
            end
            draw_rounded_rectangle(false, x, y, w, h, r, nil,
                                   :clip => clip_block)
          end
          draw_rounded_rectangle(false, x, y, w, h, r, "red")
        end

        def magnifying?
          magnifier_action.active?
        end

        def magnifier_action
          @canvas.action("ToggleMagnifier")
        end
      end
    end
  end
end
