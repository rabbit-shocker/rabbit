require "rabbit/utils"

require "rabbit/image"
require "rabbit/element"
require "rabbit/parser/base"

module Rabbit
  module Parser
    class Image < Base
      push_loader(self)

      class << self
        def match?(source)
          begin
            Rabbit::TemporaryFile.make(source.read, "image") do |input|
              Rabbit::ImageLoader.new(input.path)
            end
            true
          rescue Rabbit::ImageLoadError
            false
          end
        end
      end

      include Element
      def parse
        TemporaryFile.make(@source.read, "image") do |image|
          @image = image
          @canvas << ImageTitleSlide.new(@image.path)
        end
      end
    end
  end
end
