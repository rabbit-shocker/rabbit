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

        private
        def init_color
          super
          init_engine_color
        end
      end
    end
  end
end
