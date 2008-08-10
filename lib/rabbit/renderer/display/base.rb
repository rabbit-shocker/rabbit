require "rabbit/renderer/base"

module Rabbit
  module Renderer
    module Display
      module Base
        include Renderer::Base

        def initialize(*args, &block)
          @drawable = nil
          super
        end

        def width
          if @drawable
            @drawable.size[0]
          end
        end
        alias original_width width

        def height
          if @drawable
            @drawable.size[1]
          end
        end
        alias original_height height

        def redraw
          widget.queue_draw
        end

        def attach_to(window)
          @window = window

          init_menu
          init_gesture_actions
          add_widget_to_window(@window)
          widget.show
          attach_menu(@window)
          attach_key(@window)
          set_configure_event
        end

        def detach
          detach_key(@window)
          detach_menu(@window)
          widget.hide
          unless @window.destroyed?
            remove_widget_from_window(@window)
            @window.signal_handler_disconnect(@configure_signal_id)
          end
          @window = nil
        end

        def toggle_whiteout
          super
          update_menu
        end

        def toggle_blackout
          super
          update_menu
        end

        def make_layout(text)
          attrs, text = Pango.parse_markup(text)
          layout = create_pango_layout(text)
          layout.set_attributes(attrs)
          layout
        end

        def create_pango_context
          widget.create_pango_context
        end

        def create_pango_layout(text)
          widget.create_pango_layout(text)
        end

        def update_title
          @canvas.update_title(@canvas.slide_title)
        end

        private
        def set_configure_event
          id = @window.signal_connect("configure_event") do |widget, event|
            configured(event.x, event.y, event.width, event.height)
            false
          end
          @configure_signal_id = id
        end

        def configured(x, y, w, h)
        end

        def queue_draw
          widget.queue_draw
        end
      end
    end
  end
end
