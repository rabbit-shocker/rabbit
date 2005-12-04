require 'gtk2'

module Rabbit
  class Graffiti
    def initialize
      clear
    end

    def button_press(x, y, width, height)
      @pressed = true
      @segments << [[x.to_f / width, y.to_f / height]]
    end

    def button_release(x, y, width, height)
      @pressed = false
    end

    def button_motion(x, y, width, height)
      if @pressed
        @segments.last << [x.to_f / width, y.to_f / height]
      end
    end

    def draw_last_segment(drawable, foreground)
      points = @segments.last
      if points.size >= 2
        width, height = drawable.size
        prev, current = points[-2..-1]
        prev_x, prev_y = prev
        x, y = current
        drawable.draw_line(foreground,
                           prev_x * width, prev_y * height,
                           x * width, y * height)
      end
    end
    
    def draw_all_segment(drawable, foreground)
      width, height = drawable.size
      @segments.each do |points|
        prev_x, prev_y = points.first
        prev_x *= width
        prev_y *= height
        points[1..-1].each do |x, y|
          x *= width
          y *= height
          drawable.draw_line(foreground, prev_x, prev_y, x, y)
          prev_x, prev_y = x, y
        end
      end
    end

    def dragging?
      @pressed
    end

    def clear
      @pressed = false
      @segments = []
    end

    def undo
      @segments.pop
    end
  end
end
