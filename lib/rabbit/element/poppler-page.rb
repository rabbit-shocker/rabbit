require 'rabbit/element/block-element'

module Rabbit
  module Element
    class PopplerPage
      include Base
      include BlockElement
      include BlockHorizontalCentering

      def initialize(page)
        @page = page
        super()
      end

      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          canvas.draw_poppler_page(@page, x, y, :width => w, :height => h)
        end
        [x, y + height, w, h - height]
      end

      def title
        text.split(/\r?\n/, 2).first
      end

      def text
        rectangle = Poppler::Rectangle.new(0, 0, @width, @height)
        @page.get_text(rectangle)
      end

      def clear_theme
        super
        @width, @height = @page.size
      end
    end
  end
end
