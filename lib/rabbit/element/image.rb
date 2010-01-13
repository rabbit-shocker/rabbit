require 'rabbit/image'
require 'rabbit/element/block-element'

module Rabbit
  module Element
    class Image
      include BlockElement
      include BlockHorizontalCentering

      alias element_draw draw
      include ImageManipulable
      alias image_draw draw
      alias draw element_draw

      attr_reader :filename
      attr_reader :caption
      attr_reader :normalized_width, :normalized_height
      attr_reader :relative_width, :relative_height

      def initialize(filename, prop)
        @filename = filename
        super(filename, prop)
        %w(caption dither_mode).each do |name|
          instance_variable_set("@#{name}", prop[name])
        end
        %w(keep_scale keep_ratio).each do |name|
          unless prop[name].nil?
            self.keep_ratio = true_value?(prop[name])
          end
        end
        %w(as_large_as_possible).each do |name|
          instance_variable_set("@#{name}", true_value?(prop[name]))
        end
        %w(width height
           x_dither y_dither
           normalized_width normalized_height
           relative_width relative_height
          ).each do |name|
          begin
            instance_variable_set("@#{name}", prop[name] && Integer(prop[name]))
          rescue ArgumentError
            raise InvalidImageSizeError.new(filename, name, prop[name])
          end
        end
        setup_draw_parameters(prop)
        resize(@width, @height)
      end

      def draw_element(canvas, x, y, w, h, simulation)
        draw_image(canvas, x, y, w, h, simulation)
      end

      def text
        @caption.to_s
      end

      def to_rd
        text
      end

      def to_html(generator)
        src = generator.save_pixbuf(pixbuf, File.basename(@filename))
        html = "<img "
        if @caption
          alt = generator.h(@caption)
          html << "title=\"#{alt}\" alt=\"#{alt}\" "
        end
        html << "src=\"#{src}\" />"
        html
      end

      def dither_mode
        @dither_mode ||= "normal"
        mode_name = "DITHER_#{@dither_mode.upcase}"
        if Gdk::RGB.const_defined?(mode_name)
          Gdk::RGB.const_get(mode_name)
        else
          Gdk::RGB::DITHER_NORMAL
        end
      end

      def x_dither
        @x_dither || 0
      end

      def y_dither
        @y_dither || 0
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
        super + @padding_left + @padding_right
      end

      def height
        super + @padding_top + @padding_bottom
      end

      def as_large_as_possible?
        @as_large_as_possible
      end

      private
      def setup_draw_parameters(prop)
        @draw_parameters = {}

        @draw_parameters[:reflect] = {} if true_value?(prop["reflect"])
        [:ratio, :alpha].each do |key|
          name = "reflect_#{key}"
          value = prop[name]
          next unless value
          @draw_parameters[:reflect] ||= {}
          @draw_parameters[:reflect][key] = Float(value)
        end

        alpha = prop["alpha"]
        @draw_parameters[:alpha] = Float(alpha) if alpha
      end

      def draw_image(canvas, x, y, w, h, simulation)
        unless simulation
          image_draw(canvas, x, y, @draw_parameters)
        end
        [x, y + height, w, h - height]
      end

      def adjust_size(canvas, x, y, w, h)
        base_w = w
        base_h = (@oh || h) - @padding_top - @padding_bottom
        if @as_large_as_possible
          iw = base_w - x
          ih = base_h - y
          if iw.to_f / original_width > ih.to_f / original_height
            iw = nil
          else
            ih = nil
          end
        else
          nw = make_normalized_size(@normalized_width)
          nh = make_normalized_size(@normalized_height)
          rw = make_relative_size(@relative_width, base_w)
          rh = make_relative_size(@relative_height, base_h)
          iw = nw || rw
          ih = nh || rh
        end
        resize(iw, ih)
      end

      def make_normalized_size(size)
        size && screen_size(size)
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
