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
        Print.printable?
      end

      def printable_renderer(slides_per_page)
        raise NoPrintSupportError unless printable?
        if slides_per_page > 1
          Print::Multiple
        else
          Print
        end
      end
    end
  end
end
