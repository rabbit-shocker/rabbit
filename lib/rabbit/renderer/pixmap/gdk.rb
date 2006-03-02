require "rabbit/renderer/engine/gdk"
require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    module Pixmap
      class GDK
        include Engine::GDK
        include Base

        class << self
          def priority
            0
          end
        end
      end
    end
  end
end
