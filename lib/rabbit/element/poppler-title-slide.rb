require 'rabbit/element/title-slide'
require 'rabbit/element/poppler-page'

module Rabbit
  module Element
    class PopplerTitleSlide < TitleSlide

      def initialize(page, document)
        @document = document
        @raw_page = page
        @page = PopplerPage.new(page)
        super(@page)
      end

      def title
        (@document.title || super).chomp
      end

      def theme
        super || "pdf"
      end

      def size_ratio
        w, h = @raw_page.size
        w.to_f / h.to_f
      end
    end
  end
end
