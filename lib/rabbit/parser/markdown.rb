require "kramdown"
require "kramdown/parser/kramdown"

require "rabbit/parser/base"

module Kramdown
  module Parser
    class Kramdown
      alias_method :handle_extension_raw, :handle_extension
      def handle_extension(name, opts, body, type, line_no=nil)
        case name
        when "wait"
          @tree.children << Element.new(:wait, body, nil, :category => type, :location => line_no)
          true
        else
          handle_extension_raw(name, opts, body, type, line_no)
        end
      end
    end
  end
end

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
