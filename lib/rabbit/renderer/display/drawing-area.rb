require 'rabbit/renderer/engine'
require 'rabbit/renderer/display/drawing-area-base'

module Rabbit
  module Renderer
    module Display
      class DrawingArea
        include Renderer::Engine::Cairo
        include DrawingAreaBase

        class << self
          def priority
            100
          end
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
