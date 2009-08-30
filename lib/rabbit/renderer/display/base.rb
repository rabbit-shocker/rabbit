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

          set_configure_event
        end

        def detach
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
