require 'rabbit/renderer/engine'
require 'rabbit/renderer/display/drawing-area-primitive'

module Rabbit
  module Renderer
    module Display
      class DrawingAreaViewOnly
        include Renderer::Engine::Cairo
        include DrawingAreaPrimitive

        def attach_to(window, container=nil, &block)
          super
          add_widgets_to_container(@container, &block)
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

        def add_widgets_to_container(container, &block)
          if block_given?
            yield(container, @area)
          else
            container.add(@area)
          end
        end

        def remove_widgets_from_container(container)
          container.remove(@area)
        end
      end
    end
  end
end
