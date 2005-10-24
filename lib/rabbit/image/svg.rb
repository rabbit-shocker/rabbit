require "rsvg2"

require "rabbit/image/base"

module Rabbit
  module ImageManipulable

    class SVG < Base

      unshift_loader(self)

      class << self
        def match?(filename)
          File.open(filename) do |f|
            /<svg|<!DOCTYPE\s+svg/ =~ f.read(200)
          end
        end
      end

      private
      def _resize(w, h)
        @pixbuf = to_pixbuf(w, h)
      end

      def load_image(width=nil, height=nil)
        @pixbuf = to_pixbuf
        resize(width, height)
      end

      def filename
        File.expand_path(@filename)
      end

      def to_pixbuf(w=nil, h=nil)
        handle = RSVG::Handle.new
        if w or h
          handle.set_size_callback do |width, height|
            [w || width, h || height]
          end
        end
        if handle.respond_to?(:base_uri=)
          handle.base_uri = filename
        end
        File.open(filename, "rb") do |f|
          handle.write(f.read)
        end
        handle.close
        handle.pixbuf
      end
    end
    
  end
end
