require 'rabbit/renderer/engine'
require 'rabbit/renderer/display/drawing-area-primitive'

module Rabbit
  module Renderer
    module Display
      class DrawingAreaViewOnly
        include Renderer::Engine.renderer_module
        include DrawingAreaPrimitive

        def attach_to(window)
          super
          @window.add(@area)# if @window # need?
        end

        def detach
          @window.remove(@area)# if @window # need?
          super
        end

        def draw_pixbuf(pixbuf, x, y, params={})
          super(pixbuf, x, y, {:draw_scaled_pixbuf => true}.merge(params))
        end

        private
        def init_color
          super
          init_engine_color
        end
      end
    end
  end
end
