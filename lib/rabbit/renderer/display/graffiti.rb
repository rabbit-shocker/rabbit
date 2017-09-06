require "forwardable"

require "rabbit/action"
require "rabbit/graffiti/processor"

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

        def attach_to(window, container=nil)
          super
          graffiti_mode_action.active = false
        end

        private
        def init_graffiti
          @graffiti = Rabbit::Graffiti::Processor.new

          pressed_button = nil
          target_button = 1

          add_button_press_hook do |event|
            pressed_button = event.button
            if graffiti_mode? and event.button == target_button
              @graffiti.button_press(event.x, event.y, width, height)
              true
            else
              false
            end
          end

          add_button_release_hook do |event, last_button_press_event|
            pressed_button = nil
            if graffiti_mode? and event.button == target_button
              @graffiti.button_release(event.x, event.y, width, height)
              Action.update_graffiti_action_status(@canvas)
              true
            else
              false
            end
          end

          add_motion_notify_hook do |event|
            if graffiti_mode? and
                @graffiti.dragging? and
                pressed_button == target_button
              @graffiti.button_motion(event.x, event.y, width, height)
              init_renderer(@drawable)
              @graffiti.draw_last_segment(self)
              finish_renderer
              true
            else
              false
            end
          end
        end

        public
        def draw_graffiti
          @graffiti.draw_all_segment(self)
        end

        def graffiti_mode_action
          @canvas.action("ToggleGraffitiMode")
        end

        def graffiti_mode?
          graffiti_mode_action.active?
        end

        def have_graffiti?
          @graffiti.have_graffiti?
        end

        def can_undo_graffiti?
          @graffiti.can_undo?
        end

        def toggle_graffiti_mode
          if graffiti_mode?
            update_cursor(:pencil)
          else
            restore_cursor(nil)
          end
          update_menu
        end

        def clear_graffiti
          @graffiti.clear
          Action.update_graffiti_action_status(@canvas)
          @area.queue_draw
        end

        def undo_graffiti
          @graffiti.undo
          Action.update_graffiti_action_status(@canvas)
          @area.queue_draw
        end

        def change_graffiti_color
          @graffiti.change_color do
            redraw
          end
        end
      end
    end
  end
end
