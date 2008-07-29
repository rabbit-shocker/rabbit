require 'gtk2'

require 'rabbit/renderer/engine'
require 'rabbit/graffiti/config-dialog'

module Rabbit
  module Graffiti
    class Processor
      DEFAULT_COLOR = Renderer::Color.parse("black")
      DEFAULT_LINE_WIDTH = 3

      attr_accessor :color, :line_width
      def initialize(default_config={})
        @default_color = default_config["color"] || DEFAULT_COLOR
        @default_line_width = default_config["line_width"] || DEFAULT_LINE_WIDTH
        clear
        clear_config
      end

      def have_graffiti?
        not @segments.empty?
      end
      
      def can_undo?
        not @undo_stack.empty?
      end
      
      def button_press(x, y, width, height)
        @pressed = true
        @undo_index = nil
        @segments << [[x.to_f / width, y.to_f / height]]
      end
      
      def button_release(x, y, width, height)
        @pressed = false
        @undo_stack << [:push]
      end
      
      def button_motion(x, y, width, height)
        if @pressed
          @segments.last << [x.to_f / width, y.to_f / height]
        end
      end
      
      def draw_last_segment(renderer)
        points = @segments.last
        if points.size >= 2
          width = renderer.width
          height = renderer.height
          prev, current = points[-2..-1]
          prev_x, prev_y = prev
          x, y = current
          renderer.draw_line(prev_x * width, prev_y * height,
                             x * width, y * height,
                             @color, {:line_width => @line_width})
        end
      end
      
      def draw_all_segment(renderer)
        return if @segments.empty?
        args = [@color, {:line_width => @line_width, :opened => true}]
        width = renderer.width
        height = renderer.height
        @segments.each do |points|
          converted_points = points.collect do |x, y|
            [x * width, y * height]
          end
          renderer.draw_lines(converted_points, *args)
        end
      end
      
      def dragging?
        @pressed
      end
      
      def clear
        @pressed = false
        @segments = []
        @undo_stack = []
        @undo_index = nil
      end

      def change_color(&block)
        dialog = Graffiti::ConfigDialog.new(@color, @line_width)
        dialog.run do |color, line_width|
          @color = color if color
          @line_width = line_width if line_width
          block.call
        end
      end

      def clear_config
        @color = @default_color
        @line_width = @default_line_width
      end

      def undo
        @undo_index ||= @undo_stack.size - 1
        command, segment = @undo_stack[@undo_index]
        case command
        when :push
          @undo_stack << [:pop, @segments.pop]
        when :pop
          @segments << segment
          @undo_stack << [:push]
        end
        
        if @undo_index > 0
          @undo_index -= 1
        else
          @undo_index = nil
        end
      end
    end
  end
end
