require "rabbit/utils"

require "rabbit/image"
require "rabbit/element"
require "rabbit/parser/base"

module Rabbit
  module Parser
    class Image < Base
      push_loader(self)

      class << self
        def format_name
          "image"
        end

        def match?(source)
          options = {
            :prefix => "image-parser-match",
            :source  => source,
          }
          Rabbit::TemporaryFile.create(options) do |input|
            begin
              Rabbit::ImageLoader.new(input.path)
              true
            rescue Rabbit::ImageLoadError
              false
            end
          end
        end
      end

      include Element
      def parse
        options = {
          :prefix => "image-parser-parse",
          :source => @source,
        }
        TemporaryFile.create(options) do |image|
          @image = image
          @canvas << ImageTitleSlide.new(@image.path)
        end
      end
    end
  end
end
