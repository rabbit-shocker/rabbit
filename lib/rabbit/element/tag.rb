require 'rabbit/element/base'

module Rabbit
  module Element
    class WaitTag
      include Base

      def have_wait_tag?
        true
      end

      def markuped_text
        ""
      end

      def draw_element(canvas, x, y, w, h, simulation)
        [x, y, w, h]
      end
    end
  end
end
