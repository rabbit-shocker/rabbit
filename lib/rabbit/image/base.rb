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
    
      attr_reader :pixbuf
    
      def initialize(filename, keep_ratio)
        @filename = filename
        @keep_ratio = keep_ratio
        load_image
        @original_pixbuf = @pixbuf
      end

      def width
        @pixbuf.width
      end
    
      def height
        @pixbuf.height
      end
    
      def resize(w, h)
        if w.nil? and h.nil?
          return
        elsif @keep_ratio
          wid = @original_pixbuf.width
          hei = @original_pixbuf.height
          if w and h.nil?
            h = (height * w.to_f / width).ceil
          elsif w.nil? and h
            w = (width * h.to_f / height).ceil
          end
        else
          w ||= width
          h ||= height
        end
        if w > 0 and h > 0 and [w, h] != [width, height]
          _resize(w, h)
        end
      end

      def load_by_pixbuf_loader(data, width=nil, height=nil)
        loader = Gdk::PixbufLoader.new
        loader.last_write(data)
        @pixbuf = loader.pixbuf
        resize(width, height)
      end
      
    end

  end
end
