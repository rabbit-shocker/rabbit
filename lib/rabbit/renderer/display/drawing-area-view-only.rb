require 'rabbit/renderer/engine'
require 'rabbit/renderer/display/drawing-area-primitive'

module Rabbit
  module Renderer
    module Display
      class DrawingAreaViewOnly
        include Renderer::Engine.renderer_module
        include DrawingAreaPrimitive

        def attach_to(window, container=nil)
          super
          add_widgets_to_container(@container)
          widget.show
        end

        def detach
          widget.hide
          unless @window.destroyed?
            remove_widgets_from_container(@container)
          end

          super
        end

        private
        def init_color
          super
          init_engine_color
        end

        def add_widgets_to_container(container)
          container.add(@area)
        end

        def remove_widgets_from_container(container)
          container.remove(@area)
        end
      end
    end
  end
end
