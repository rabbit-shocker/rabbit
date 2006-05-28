require "rabbit/utils"

Rabbit::Utils.require_safe "rabbit/renderer/engine/cairo"
require "rabbit/renderer/pixmap/base"
require "rabbit/renderer/pixmap/gl"

module Rabbit
  module Renderer
    module Pixmap
      class Cairo
        include Engine::Cairo
        include GL
        include Base

        class << self
          def priority
            100
          end
        end
      end
    end
  end
end
