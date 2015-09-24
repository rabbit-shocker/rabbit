require "kramdown"
require "kramdown/parser/kramdown"
require "kramdown/parser/gfm"

require "rabbit/parser/base"

module Kramdown
  module Parser
    class Kramdown
      alias_method :handle_extension_raw, :handle_extension
      def handle_extension(name, opts, body, type, line_no=nil)
        return true if handle_extension_raw(name, opts, body, type, line_no)
        element = Element.new(name.to_sym,
                              nil,
                              opts,
                              :category => type,
                              :location => line_no)
        if body
          root, warnings = self.class.parse(body, @options)
          fix_location(root, line_no)
          if type == :span
            p_element = root.children.first
            p_element.children.each do |sub_element|
              element.children << sub_element
            end
          else
            root.children.each do |sub_element|
              element.children << sub_element
            end
          end
        end
        @tree.children << element
        true
      end

      alias_method :configure_parser_raw, :configure_parser
      def configure_parser
        @span_parsers.unshift(:strikethrough)
        position = @block_parsers.index(:codeblock_fenced)
        @block_parsers.insert(position, :codeblock_fenced_gfm)

        configure_parser_raw
      end

      STRIKETHROUGH_DELIMITER = "~~"
      STRIKETHROUGH_DELIMITER_PATTERN =
        /#{Regexp.escape(STRIKETHROUGH_DELIMITER)}/
      def parse_strikethrough
        start_line_number = @src.current_line_number

        delimiter = @src.scan(STRIKETHROUGH_DELIMITER_PATTERN)
        saved_pos = @src.save_pos

        text = @src.scan_until(STRIKETHROUGH_DELIMITER_PATTERN)
        if text
          text = text.sub(/#{STRIKETHROUGH_DELIMITER_PATTERN}\Z/, "")
          @tree.children << Element.new(:strikethrough, text, nil,
                                        :location => start_line_number)
        else
          @src.revert_pos(saved_pos)
          add_text(delimiter)
        end
      end
      define_parser(:strikethrough,
                    STRIKETHROUGH_DELIMITER_PATTERN,
                    STRIKETHROUGH_DELIMITER)

      escaped_chars_start_re =
        parser(:escaped_chars).start_re.to_s.sub(/(\]\)\))\z/, "~\\1")
      parser(:escaped_chars).start_re = /#{escaped_chars_start_re}/


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

      private
      def fix_location(element, base_location)
        element.options[:location] += base_location - 1
        element.children.each do |sub_element|
          fix_location(sub_element, base_location)
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

      def parse
        document = Kramdown::Document.new(@source.read)
        converter = Converter.new(@canvas)
        converter.convert(document.root)
      end
    end
  end
end
