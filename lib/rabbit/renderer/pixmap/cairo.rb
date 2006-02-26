require "rabbit/renderer/engine/cairo"
require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    module Pixmap
      class Cairo
        include Base
        include Engine::Cairo
        
        class << self
          def priority
            100
          end
        end
        
        def initialize(canvas, width=nil, height=nil)
          super
        end
        
        def to_gdk_rgb(color)
          make_color(color).to_gdk_rgb
        end
        
        def draw_slide(slide, simulation)
          init_pixmap(slide, simulation)
          unless simulation
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
      end
    end
  end
end
