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
        @pixbuf = RSVG.pixbuf_from_file_at_size(filename, w, h)
      end

      def load_image(width=nil, height=nil)
        @pixbuf = RSVG.pixbuf_from_file(filename)
        resize(width, height)
      end

      def filename
        File.expand_path(@filename)
      end
    end
    
  end
end
