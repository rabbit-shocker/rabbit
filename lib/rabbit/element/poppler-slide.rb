require 'rabbit/element/slide'
require 'rabbit/element/poppler-page'

module Rabbit
  module Element
    class PopplerSlide < Slide
      def initialize(page)
        @page = PopplerPage.new(page)
        super(@page)
      end

      def pdf_page
        @page
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
    end
  end
end
