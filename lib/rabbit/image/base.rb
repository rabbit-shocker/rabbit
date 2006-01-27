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

      attr_reader :width, :height
    
      def initialize(filename, keep_ratio)
        @filename = filename
        @keep_ratio = keep_ratio
        load_image
        @width = @pixbuf.width
        @height = @pixbuf.height
        @original_pixbuf = @pixbuf
      end

      def pixbuf
        ensure_update
        @pixbuf
      end

      def original_width
        @original_pixbuf.width
      end
    
      def original_height
        @original_pixbuf.height
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
      def load_by_pixbuf_loader(data, width=nil, height=nil)
        loader = Gdk::PixbufLoader.new
        begin
          loader.last_write(data)
        rescue Gdk::PixbufError
          loader.close rescue Gdk::PixbufError
          raise ImageLoadError.new("#{@filename}: #{$!.message}")
        end
        @pixbuf = loader.pixbuf
        resize(width, height)
      end

      def ensure_update
        if [@width, @height] != [@pixbuf.width, @pixbuf.height]
          _resize(@width, @height)
        end
      end
    end
  end
end
