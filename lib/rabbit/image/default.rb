require "rabbit/image/base"

module Rabbit
  module ImageManipulable

    class Default < Base

      push_loader(self)
      
      class << self
        def match?(filename)
          true
        end
      end
      
      private
      def _resize(w, h)
        @pixbuf = @original_pixbuf.scale(w, h)
      end
      
      def load_image(width=nil, height=nil)
        File.open(@filename) do |file|
          file.binmode
          load_by_pixbuf_loader(file.read, width, height)
        end
      end
    end
    
  end
end
