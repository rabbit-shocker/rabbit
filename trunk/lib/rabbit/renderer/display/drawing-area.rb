require 'rabbit/renderer/engine'
require 'rabbit/renderer/display/drawing-area-base'
require 'rabbit/renderer/display/comment'

module Rabbit
  module Renderer
    module Display
      class DrawingArea
        include Renderer::Engine.renderer_module
        include DrawingAreaBase
        include Comment

        class << self
          def priority
            100
          end
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
