require 'rabbit/element/slide'
require 'rabbit/element/poppler-page'

module Rabbit
  module Element
    class PopplerSlide < Slide
      def initialize(page)
        @raw_page = page
        @page = PopplerPage.new(page)
        super(@page)
      end

      def headline
        @page
      end

      def body
        @page
      end

      def title
        (@page.text.split(/\r?\n/, 2)[0] || super).chomp
      end

      def size_ratio
        w, h = @raw_page.size
        w.to_f / h.to_f
      end
    end
  end
end
