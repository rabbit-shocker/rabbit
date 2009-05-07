require "gdk_pixbuf2"

require "rabbit/utils"

module Rabbit
  module ImageManipulable

    class Base
      extend ModuleLoader

      attr_reader :width, :height, :original_width, :original_height

      def initialize(filename, props)
        @filename = filename
        @props = normalize_props(props)
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
        params["width"] ||= @width
        params["height"] ||= @height
        canvas.draw_pixbuf(pixbuf, x, y, params)
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

      def load_by_pixbuf_loader(data)
        loader = Gdk::PixbufLoader.new
        id = loader.signal_connect("size_prepared") do |l, width, height|
          @width = width
          @height = height
        end
        begin
          loader.last_write(data)
        rescue Gdk::PixbufError
          loader.close rescue Gdk::PixbufError
          raise ImageLoadError.new("#{@filename}: #{$!.message}")
        end
        loader.signal_handler_disconnect(id)
        loader
      end
    end
  end
end
