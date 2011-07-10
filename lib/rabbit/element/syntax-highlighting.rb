require 'rabbit/element/preformatted'

module Rabbit
  module Element
    class SyntaxHighlightingBlock < PreformattedBlock
      private
      def attributes
        " class=\"syntax-highlighting\""
      end
    end

    class SyntaxHighlightingText < PreformattedText
    end
  end
end
