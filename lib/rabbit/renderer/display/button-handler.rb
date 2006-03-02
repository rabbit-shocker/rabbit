require 'gtk2'

require 'rabbit/renderer/display/menu'

module Rabbit
  module Renderer
    module Display
      module ButtonHandler
        include Menu

        BUTTON_PRESS_ACCEPTING_TIME = 250

        def initialize(*args, &block)
          super
          init_button_handler
        end

        private
        def init_button_handler
          @button_handing = false
          clear_button_handler
        end

        def clear_button_handler
          @button_handler = []
        end

        def set_button_event(widget)
          last_button_press_event = nil
          widget.signal_connect("button_press_event") do |_widget, event|
            last_button_press_event = event
            call_hook_procs(@button_press_hook_procs, event)
          end

          widget.signal_connect("button_release_event") do |_widget, event|
            handled = call_hook_procs(@button_release_hook_procs,
                                      event, last_button_press_event)
            if handled
              clear_button_handler
            else
              handled = handle_button_release(event, last_button_press_event)
            end
            handled
          end
        end

        BUTTON_PRESS_HANDLER = {
          Gdk::Event::Type::BUTTON_PRESS => "handle_button_press",
          Gdk::Event::Type::BUTTON2_PRESS => "handle_button2_press",
          Gdk::Event::Type::BUTTON3_PRESS => "handle_button3_press",
        }

        def handle_button_release(event, last_button_press_event)
          press_event_type = last_button_press_event.event_type
          if BUTTON_PRESS_HANDLER.has_key?(press_event_type)
            __send__(BUTTON_PRESS_HANDLER[press_event_type],
                     last_button_press_event, event)
            start_button_handler
          end
          true
        end

        def handle_button_press(event, release_event)
          case event.button
          when 1, 5
            unless release_event.state.mod1_mask?
              add_button_handler do
                @canvas.activate("NextSlide")
              end
            end
          when 2, 4
            unless release_event.state.mod1_mask?
              add_button_handler do
                @canvas.activate("PreviousSlide")
              end
            end
          when 3
            add_button_handler do
              popup_menu
            end
          end
        end

        def handle_button2_press(event, release_event)
          add_button_handler do
            if @canvas.index_mode?
              index = @canvas.current_slide.slide_number(@canvas,
                                                         event.x,
                                                         event.y)
              if index
                @canvas.activate("ToggleIndexMode")
                @canvas.activate("JumpTo") {index}
              end
            end
            clear_button_handler
          end
        end

        def handle_button3_press(event, release_event)
          add_button_handler do
            clear_button_handler
          end
        end

        def add_button_handler(handler=Proc.new)
          @button_handler.push(handler)
        end

        def call_button_handler
          @button_handler.pop.call until @button_handler.empty?
        end

        def start_button_handler
          if @button_handling
            @button_event_coming = true
          else
            @button_handling = true
            @button_event_coming = false
            Gtk.timeout_add(BUTTON_PRESS_ACCEPTING_TIME) do
              if @button_event_coming
                Gtk.timeout_add(BUTTON_PRESS_ACCEPTING_TIME) do
                  call_button_handler
                  @button_handling = false
                  false
                end
              else
                call_button_handler
                @button_handling = false
              end
              false
            end
          end
        end
      end
    end
  end
end
