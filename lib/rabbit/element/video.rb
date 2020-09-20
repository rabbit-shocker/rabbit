require "rabbit/utils"

require "rabbit/image"
require "rabbit/element/block-element"
require "rabbit/element/text-renderer"
require "rabbit/parser/base"

module Rabbit
  module Element
    class Video
      include Base
      include BlockElement
      include BlockHorizontalCentering
      include TextRenderer

      attr_reader :filename
      attr_reader :relative_width, :relative_height
      attr_reader :relative_margin_top, :relative_margin_bottom
      attr_reader :relative_margin_left, :relative_margin_right
      attr_reader :relative_padding_top, :relative_padding_bottom
      attr_reader :relative_padding_left, :relative_padding_right

      def initialize(filename, prop)
        @filename = filename
        prop = Utils.stringify_hash_key(prop)
        super()
        normalized_prop = {}
        prop.each do |name, value|
          normalized_prop[name.gsub(/-/, '_')] = value
        end
        prop = normalized_prop
        %w(as_large_as_possible).each do |name|
          instance_variable_set("@#{name}", true_value?(prop[name]))
        end
        %w(width height
           relative_width relative_height
           relative_margin_top relative_margin_bottom
           relative_margin_left relative_margin_right
           relative_padding_top relative_padding_bottom
           relative_padding_left relative_padding_right
          ).each do |name|
          begin
            instance_variable_set("@#{name}", prop[name] && Integer(prop[name]))
          rescue ArgumentError
            raise InvalidImageSizeError.new(filename, name, prop[name])
          end
        end

        resize(@width, @height)
      end

      alias _compile compile
      def compile_for_horizontal_centering(canvas, x, y, w, h)
        _compile(canvas, x, y, w, h)
      end

      def compile(canvas, x, y, w, h)
        super
        adjust_size(canvas, @x, @y, @w, @h)
      end

      def width
        @width.to_i + @padding_left + @padding_right
      end

      def height
        @height.to_i + @padding_top + @padding_bottom
      end

      def as_large_as_possible?
        @as_large_as_possible
      end

      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          if canvas.display?
            require "rabbit/video-window"
            VideoWindow.show(canvas.window, self)
          else
            draw_layout(canvas, x, y)
          end
        end
        [x, y + height, w, h - height]
      end

      def text
        "video : #{File.basename(@filename)}"
      end

      def to_rd
        text
      end

      private
      def adjust_margin(w, h)
        @margin_top =
          make_relative_size(@relative_margin_top, h) || @margin_top
        @margin_bottom =
          make_relative_size(@relative_margin_bottom, h) || @margin_bottom
        @margin_left =
          make_relative_size(@relative_margin_left, w) || @margin_left
        @margin_right =
          make_relative_size(@relative_margin_right, w) || @margin_right
      end

      def adjust_padding(w, h)
        @padding_top =
          make_relative_size(@relative_padding_top, h) || @padding_top
        @padding_bottom =
          make_relative_size(@relative_padding_bottom, h) || @padding_bottom
        @padding_left =
          make_relative_size(@relative_padding_left, w) || @padding_left
        @padding_right =
          make_relative_size(@relative_padding_right, w) || @padding_right
      end

      def adjust_size(canvas, x, y, w, h)
        base_w = w
        base_h = h
        adjust_margin(base_w, base_h)
        adjust_padding(base_w, base_h)
        base_h = base_h - @padding_top - @padding_bottom
        if @as_large_as_possible
          iw = base_w
          ih = base_h
        else
          rw = make_relative_size(@relative_width, base_w)
          rh = make_relative_size(@relative_height, base_h)
          iw = rw || base_w
          ih = rh || base_h
        end
        resize(iw, ih)
      end

      def resize(w, h)
        if w.nil? and h.nil?
          return
        else
          w ||= width
          h ||= height
        end
        w = w.ceil if w
        h = h.ceil if h
        if w and w > 0 and h and h > 0 and [w, h] != [width, height]
          @width = w
          @height = h
        end
      end

      def make_relative_size(size, parent_size)
        size && parent_size && ((size / 100.0) * parent_size).ceil
      end

      def true_value?(value)
        value == true or value == "true"
      end
    end
  end
end
