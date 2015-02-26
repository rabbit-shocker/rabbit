require "rabbit/renderer/engine/cairo"
require "rabbit/renderer/pixmap/base"
require "rabbit/renderer/pixmap/gl"

module Rabbit
  module Renderer
    module Pixmap
      class Cairo
        include Engine::Cairo
        include GL
        include Base
      end
    end
  end
end
