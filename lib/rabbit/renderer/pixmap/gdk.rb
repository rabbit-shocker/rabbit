require "rabbit/renderer/engine/gdk"
require "rabbit/renderer/pixmap/base"
require "rabbit/renderer/pixmap/gl"

module Rabbit
  module Renderer
    module Pixmap
      class GDK
        include Engine::GDK
        include GL
        include Base

        class << self
          def priority
            0
          end
        end

        # force disable GL support because there is a
        # problem: Rendered objects by GL aren't showed when
        # the objects are rendered after
        # draw_layout(). Why???
        def gl_supported?
          false
        end
      end
    end
  end
end
