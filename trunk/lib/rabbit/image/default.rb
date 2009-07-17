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
      def update_size
        File.open(@filename, "rb") do |file|
          Dir.chdir(File.dirname(@filename)) do
            loader = load_by_pixbuf_loader(file.read)
            @pixbuf = loader.pixbuf
          end
        end
      end
    end
  end
end
