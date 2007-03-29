require 'rabbit/element/title-slide'
require 'rabbit/element/poppler-page'

module Rabbit
  module Element
    class PopplerTitleSlide < TitleSlide

      def initialize(page, document)
        @document = document
        @page = PopplerPage.new(page)
        super(@page)
      end

      def title
        @document.title || super
      end

      def theme
        super || "pdf"
      end
    end
  end
end
