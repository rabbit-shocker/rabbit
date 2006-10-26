module Rabbit
  module Renderer
    module Display
      module Spotlight
        def initialize(canvas)
          super
          init_spotlight
        end

        private
        def init_spotlight
          @spotlighting = false

          @spotlight_radius_ratio = 0.1

          motioned = false
          target_button = 3
          target_event_type = Gdk::Event::BUTTON2_PRESS
          target_info = [target_button, target_event_type]

          add_button_press_hook do |event|
            if [event.button, event.event_type] == target_info
              add_button_handler do
                if spotlighting?
                  Gtk.grab_remove(@area)
                  Gdk.pointer_ungrab(event.time)
                else
                  @spotlight_center_x = event.x
                  @spotlight_center_y = event.y
                  Gtk.grab_add(@area)
                  Gdk.pointer_grab(@area.window, false,
                                   Gdk::Event::BUTTON_PRESS_MASK |
                                   Gdk::Event::BUTTON_RELEASE_MASK |
                                   Gdk::Event::SCROLL_MASK |
                                   Gdk::Event::POINTER_MOTION_MASK,
                                   nil, nil,
                                   event.time)
                end
                motioned = false
                @spotlighting = !@spotlighting
                clear_button_handler
                queue_draw
                true
              end
            end
            false
          end

          add_motion_notify_hook do |event|
            if spotlighting?
              @spotlight_center_x = event.x
              @spotlight_center_y = event.y
              queue_draw
              motioned = true
              true
            else
              false
            end
          end

          add_scroll_hook do |event|
            if spotlighting?
              case event.direction
              when Gdk::EventScroll::Direction::UP
                @spotlight_radius_ratio =
                  [1, @spotlight_radius_ratio + 0.1].min
              when Gdk::EventScroll::Direction::DOWN
                @spotlight_radius_ratio =
                  [0.1, @spotlight_radius_ratio - 0.1].max
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
          draw_rectangle(true, 0, 0, width, height, nil, params)
        end

        def spotlighting?
          @spotlighting
        end
      end
    end
  end
end
