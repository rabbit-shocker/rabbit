require "gtk2"

require "rabbit/rabbit"
require "rabbit/gettext"

module Rabbit
  module Renderer

    module Base
      include GetText

      def initialize(canvas)
        @canvas = canvas
        @font_families
      end
      
      def font_families
        if @font_families.nil? or @font_families.empty?
          layout = create_dummy_pango_layout
          @font_families = layout.context.list_families
        end
        @font_families
      end
      
    end
    
  end
end
