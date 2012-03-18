require "kramdown"

require "rabbit/parser/base"

module Rabbit
  module Parser
    class Markdown < Base
    end
  end
end

require "rabbit/parser/markdown/converter"

module Rabbit
  module Parser
    class Markdown
      unshift_loader(self)
      class << self
        def match?(source)
          /\A(?:md|markdown)\z/i =~ source.extension.to_s
        end
      end

      include Element
      def parse
        document = Kramdown::Document.new(@source.read)
        converter = Converter.new(@canvas)
        converter.convert(document.root)
      end
    end
  end
end
