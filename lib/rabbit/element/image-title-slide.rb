require 'rabbit/element/title-slide'

module Rabbit
  module Element
    class ImageTitleSlide < TitleSlide

      def initialize(image_path)
        @image_path = image_path
        super(Element::Image.new(image_path, {}))
      end

      def theme
        super || "image-viewer"
      end
    end
  end
end
