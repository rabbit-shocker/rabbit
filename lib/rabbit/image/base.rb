require "gdk_pixbuf2"

require "rabbit/utils"
require "rabbit/image-data-loader"

module Rabbit
  module ImageManipulable

    class Base
      extend ModuleLoader

      attr_reader :width, :height, :original_width, :original_height
      attr_reader :animation

      def initialize(filename, props)
        @filename = filename
        @props = normalize_props(props)
        @animation = nil
        @animation_iterator = nil
        update_size
        @original_width = @width
        @original_height = @height
      end

      def [](key)
        @props[normalize_prop_key(key)]
      end

      def []=(key, value)
        @props[normalize_prop_key(key)] = value
      end

      def keep_ratio
        self["keep_ratio"]
      end

      def keep_ratio=(value)
        self["keep_ratio"] = value
      end

      def pixbuf
        @pixbuf
      end

      def resize(w, h)
        if w.nil? and h.nil?
          return
        elsif keep_ratio
          if w and h.nil?
            h = (original_height * w.to_f / original_width).ceil
          elsif w.nil? and h
            w = (original_width * h.to_f / original_height).ceil
          end
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

      def draw(canvas, x, y, params={})
        default_params = {
          :width => width,
          :height => height,
        }
        target_pixbuf = pixbuf
        if @animation_iterator
          @animation_iterator.advance
          target_pixbuf = @animation_iterator.pixbuf
          delay_time = @animation_iterator.delay_time
          if delay_time > 0
            GLib::Timeout.add(delay_time) do
              canvas.redraw
              GLib::Source::REMOVE
            end
          end
        end
        canvas.draw_pixbuf(target_pixbuf, x, y, default_params.merge(params))
      end

      private
      def normalize_props(props)
        normalized_props = {}
        (props || {}).each do |key, value|
          normalized_props[normalize_prop_key(key)] = value
        end
        keep_ratio_key = normalize_prop_key("keep_ratio")
        unless normalized_props.has_key?(keep_ratio_key)
          normalized_props[keep_ratio_key] = true
        end
        normalized_props
      end

      def normalize_prop_key(key)
        key.to_s.gsub(/-/, "_")
      end

      def load_data(data)
        loader = ImageDataLoader.new(data)
        begin
          loader.load
        rescue ImageLoadError
          raise ImageLoadError.new("#{@filename}: #{$!.message}")
        end

        @width = loader.width
        @height = loader.height
        @pixbuf = loader.pixbuf
        @animation = loader.animation
        if @animation and not @animation.static_image?
          @animation_iterator = @animation.get_iter
        else
          @animation_iterator = nil
        end
      end
    end
  end
end
