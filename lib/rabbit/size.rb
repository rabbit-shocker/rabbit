module Rabbit
  class Size
    attr_reader :real_width
    attr_reader :real_height
    attr_reader :logical_width
    attr_reader :logical_height
    attr_reader :logical_margin_left
    attr_reader :logical_margin_right
    attr_reader :logical_margin_top
    attr_reader :logical_margin_bottom
    attr_reader :ratio
    def initialize(width, height, ratio)
      @real_width = width
      @real_height = height
      @ratio = ratio
      compute_logical_size
    end

    def have_logical_margin_x?
      @logical_margin_left > 0 or
        @logical_margin_right > 0
    end

    def have_logical_margin_y?
      @logical_margin_top > 0 or
        @logical_margin_bottom > 0
    end

    def have_logical_margin?
      have_logical_margin_x? or have_logical_margin_y?
    end

    private
    def compute_logical_size
      real_ratio = @real_width.to_f / @real_height.to_f
      if real_ratio == @ratio
        @logical_width = @real_width
        @logical_height = @real_height
        @logical_margin_left = 0
        @logical_margin_right = 0
        @logical_margin_top = 0
        @logical_margin_bottom = 0
        return
      end

      if real_ratio > @ratio
        @logical_width = @real_width * (@ratio / real_ratio)
        @logical_height = @real_height
        width_margin = @real_width - @logical_width
        @logical_margin_left = width_margin / 2
        @logical_margin_right = width_margin / 2
        @logical_margin_top = 0
        @logical_margin_bottom = 0
      else
        @logical_width = @real_width
        @logical_height = @real_height * (real_ratio / @ratio)
        height_margin = @real_height - @logical_height
        @logical_margin_left = 0
        @logical_margin_right = 0
        @logical_margin_top = height_margin / 2
        @logical_margin_bottom = height_margin / 2
      end
    end
  end
end
