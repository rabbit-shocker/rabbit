require "kramdown"
require "kramdown/parser/kramdown"
require "kramdown/parser/gfm"

require "rabbit/parser/base"

module Kramdown
  module Parser
    class Kramdown
      alias_method :handle_extension_raw, :handle_extension
      def handle_extension(name, opts, body, type, line_no=nil)
        case name
        when "wait"
          @tree.children << Element.new(:wait, body, nil,
                                        :category => type,
                                        :location => line_no)
          true
        else
          handle_extension_raw(name, opts, body, type, line_no)
        end
      end

      alias_method :configure_parser_raw, :configure_parser
      def configure_parser
        position = @block_parsers.index(:codeblock_fenced)
        @block_parsers.insert(position, :codeblock_fenced_gfm)

        configure_parser_raw
      end

      parser(:codeblock_fenced_gfm).method = "parse_codeblock_fenced_gfm"

      def parse_codeblock_fenced_gfm
        original_match = self.class::FENCED_CODEBLOCK_MATCH
        begin
          self.class.send(:remove_const, :FENCED_CODEBLOCK_MATCH)
          self.class.const_set(:FENCED_CODEBLOCK_MATCH,
                               GFM::FENCED_CODEBLOCK_MATCH)
          parse_codeblock_fenced
        ensure
          self.class.send(:remove_const, :FENCED_CODEBLOCK_MATCH)
          self.class.const_set(:FENCED_CODEBLOCK_MATCH, original_match)
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
        def format_name
          "Makrdown"
        end

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
