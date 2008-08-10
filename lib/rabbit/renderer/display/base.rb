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

        private
        def configured(x, y, w, h)
        end
      end
    end
  end
end
