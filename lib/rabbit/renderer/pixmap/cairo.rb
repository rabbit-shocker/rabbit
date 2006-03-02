require "rabbit/renderer/engine/cairo"
require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    module Pixmap
      class Cairo
        include Engine::Cairo
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
