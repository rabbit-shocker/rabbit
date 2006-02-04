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
      def ensure_resize(w, h)
        @pixbuf = @original_pixbuf.scale(w, h)
      end

      def update_size
        File.open(@filename, "rb") do |file|
          loader = load_by_pixbuf_loader(file.read)
          @original_pixbuf = loader.pixbuf
        end
      end
    end
  end
end
