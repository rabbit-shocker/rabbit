require "rabbit/renderer/base"
require "rabbit/renderer/display/hook-handler"

module Rabbit
  module Renderer
    module Display
      module Base
        include Renderer::Base
        include HookHandler

        def initialize(*args, &block)
          @drawable = nil
          super
        end

        def width
          if @drawable
            @drawable.width
          end
        end
        alias original_width width

        def height
          if @drawable
            @drawable.height
          end
        end
        alias original_height height

        def redraw
          widget.queue_draw
        end

        def attach_to(window, container=nil)
          @window = window
          @container = container || @window

          set_configure_event
        end

        def detach
          if !@window.destroyed? and @configure_signal_id
            @window.signal_handler_disconnect(@configure_signal_id)
            @configure_signal_id = nil
          end

          @window = nil
          @container = nil
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
          context = widget.create_pango_context
          set_font_resolution(context)
          context
        end

        def create_pango_layout(text)
          layout = widget.create_pango_layout(text)
          set_font_resolution(layout.context)
          layout
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
