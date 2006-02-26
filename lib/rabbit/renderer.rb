require "rabbit/utils"

require "rabbit/renderer/color"
require "rabbit/renderer/drawing-area"
require "rabbit/renderer/pixmap"
require "rabbit/renderer/print"

module Rabbit
  module Renderer
    extend Utils

    class << self
      def printable?
        Print.printable?
      end

      def printable_renderer(slides_per_page)
        if slides_per_page > 1
          Print::Multiple
        else
          Print
        end
      end
    end
  end
end
