require "rabbit/renderer/cairo"
require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    module Pixmap
      class Cairo
        include Base
        include Renderer::Cairo
        
        class << self
          def priority
            100
          end
        end
        
        def initialize(canvas, width=nil, height=nil)
          super
        end
        
        def foreground=(color)
          @foreground = color
        end
        
        def background=(color)
          @background = color
        end
        
        def background_image=(pixbuf)
          @background_image = pixbuf
        end
        
        def to_gdk_rgb(color)
          make_color(color).to_gdk_rgb
        end
        
        def draw_slide(slide, simulation)
          init_pixmap(slide, simulation)
          unless simulation
            @context = drawable.create_cairo_context
            draw_rectangle(true, 0, 0, width, height, @background)
          end
          yield
        end

        private
        def init_color
          super
          @foreground = make_color("black")
          @background = make_color(@background_color)
        end

        def init_drawable
          super
          @context = drawable.create_cairo_context
        end
      end
    end
  end
end
