# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require "rd/rdfmt"

require_relative "base"

module Rabbit
  module Parser
    class RD < Base
    end
  end
end

require_relative "rd/rd2rabbit-lib"

module Rabbit
  module Parser
    class RD
      push_loader(self)
      class << self
        def format_name
          "RD"
        end

        def match?(source)
          extension = source.extension
          if extension.nil?
            head = source.read[0, 500]
            if head.respond_to?(:force_encoding)
              head.force_encoding("ASCII-8BIT")
            end
            /^=(?:\s+\S|[^=])/ === head
          else
            /\A(?:rd|rab|rbt)\z/i =~ extension
          end
        end
      end

      def parse
        source = @source.read.gsub(/\r\n/, "\n")
        source = "=begin\n#{source}\n=end\n"
        tree = ::RD::RDTree.new(source)
        visitor = RD2RabbitVisitor.new(@canvas, @progress)
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
        lines = source.lines.to_a[0..-2]
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
