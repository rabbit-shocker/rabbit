require "gdk_pixbuf2"

module Rabbit
  module ImageManipulable

    class Base

      @@loaders = []

      class << self
        def unshift_loader(loader)
          @@loaders.unshift(loader)
        end

        def push_loader(loader)
          @@loaders.push(loader)
        end

        def find_loader(filename)
          @@loaders.find do |loader|
            loader.match?(filename)
          end
        end
      end
      
      attr_accessor :keep_ratio

      attr_reader :width, :height, :original_width, :original_height
    
      def initialize(filename, keep_ratio)
        @filename = filename
        @keep_ratio = keep_ratio
        update_size
        @original_width = @width
        @original_height = @height
      end

      def pixbuf
        ensure_update
        internal_pixbuf
      end

      def resize(w, h)
        if w.nil? and h.nil?
          return
        elsif @keep_ratio
          if w and h.nil?
            h = (original_height * w.to_f / original_width).ceil
          elsif w.nil? and h
            w = (original_width * h.to_f / original_height).ceil
          end
        else
          w ||= width
          h ||= height
        end
        if w and w > 0 and h and h > 0 and [w, h] != [width, height]
          @width = w
          @height = h
        end
      end

      private
      def load_by_pixbuf_loader(data)
        loader = Gdk::PixbufLoader.new
        loader.signal_connect("size_prepared") do |l, width, height|
          @width = width
          @height = height
        end
        begin
          loader.last_write(data)
        rescue Gdk::PixbufError
          loader.close rescue Gdk::PixbufError
          raise ImageLoadError.new("#{@filename}: #{$!.message}")
        end
        loader
      end

      def ensure_update
        _pixbuf = internal_pixbuf
        if _pixbuf.nil? or [@width, @height] != [_pixbuf.width, _pixbuf.height]
          ensure_resize(@width, @height)
        end
      end

      def internal_pixbuf
        @pixbuf
      end
    end
  end
end
