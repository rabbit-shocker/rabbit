require "rabbit/parser/rd/rd2rabbit-lib"

module Rabbit
  module Parser
    class RD
      def initialize(canvas, source)
        @canvas = canvas
        @source = source
      end

      def parse
        source = "=begin\n#{@source.read}\n=end\n"
        tree = ::RD::RDTree.new(source)
        visitor = RD2RabbitVisitor.new(@canvas)
        visitor.visit(tree)
      rescue Racc::ParseError
        message = format_parse_error_message($!.message, source)
        raise ParseError.new, message, $@
      end

      private
      def format_parse_error_message(message, source)
        if /line (\d+):/.match(message)
          numbered_source = add_number(source, $1.to_i)
        else
          numbered_source = add_number(source)
        end
        "#{message}\n--\n#{numbered_source}"
      end

      SNIPPET_SIZE = 10
      def add_number(source, around=nil)
        i = 1
        puts source
        lines = source.to_a[0..-2]
        if around
          i = [1, around - SNIPPET_SIZE].max
          lines = lines[i, 2 * SNIPPET_SIZE]
        end
        format = "%#{Math.log10(lines.size).truncate + 1}d %s"

        lines.collect do |line|
          i += 1
          format % [i, line]
        end.join
      end
    end
  end
end
