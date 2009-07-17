require 'rabbit/element/title-slide'

module Rabbit
  module Element
    class ImageTitleSlide < TitleSlide

      def initialize(image_path, properties=nil)
        @image_path = image_path
        properties ||= {:as_large_as_possible => true}
        super(Element::Image.new(image_path, properties))
      end

      def theme
        super || "image-viewer"
      end
    end
  end
end
