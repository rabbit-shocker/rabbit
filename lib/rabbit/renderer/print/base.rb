require "rabbit/rabbit"

require "rabbit/renderer/base"

module Rabbit
  module Renderer
    module Print
      module Base
        include Renderer::Base

        attr_accessor :filename, :show_page
        
        def initialize(canvas)
          super
          @filename = nil
        end

        def width
          page_width
        end

        def height
          page_height
        end

        def pre_print(slide_size)
          @show_page = true
        end

        def printable?
          true
        end
      end
    end
  end
end

