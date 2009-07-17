require 'rabbit/element/base'

module Rabbit
  module Element
    module BlockHorizontalCentering
      attr_reader :ox, :oy, :ow, :oh # dirty!!!!

      def do_horizontal_centering(canvas, x, y, w, h)
        @ox, @oy, @ow, @oh = @x, @y, @w, @h
        adjust_width = ((w - width) / 2.0).ceil
        x += adjust_width
        w -= adjust_width
        @centering_adjusted_width = adjust_width
        compile_for_horizontal_centering(canvas, x, @y, w, h)
        draw(true)
      end

      def clear_theme
        @ox = @oy = @ow = @oh = nil
        super
      end
    end

    module BlockElement
      include Base

      def inline_element?
        false
      end

      def adjust_y_padding(y, h)
        y += @padding_bottom
        h -= @padding_bottom
        [y, h]
      end
    end
  end
end
