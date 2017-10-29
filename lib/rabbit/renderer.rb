require "rabbit/utils"

require "rabbit/renderer/color"
require "rabbit/renderer/display"
require "rabbit/renderer/offscreen"
require "rabbit/renderer/print"

module Rabbit
  module Renderer
    extend Utils

    class << self
      def printable?
        true
      end

      def printable_renderer(slides_per_page)
        if slides_per_page > 1
          Print::Multiple
        else
          Print::Cairo
        end
      end
    end
  end
end
