require 'erb'
require 'rd/rdfmt'
require 'rd/rd2html-lib'

module Jekyll
  class RDConverter < Converter
    safe true

    def matches(ext)
      /rd/i =~ ext
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      setup
      source = content.gsub(/\r\n/, "\n")
      source = "=begin\n#{source}\n=end\n"
      tree = RD::RDTree.new(source)
      visitor = RD2HTMLSnippetVisitor.new
      visitor.visit(tree)
    rescue Racc::ParseError
      format_parse_error_message($!.message, source)
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

    class RD2HTMLSnippetVisitor < RD::RD2HTMLVisitor
      def apply_to_DocumentElement(element, contents)
        content = contents.join("\n")
        foottext = make_foottext
        snippet = "<section class=\"contents\">\n#{content}\n</section>\n"
        if foottext
          snippet << "<section class=\"foottext\">#{foottext}</section>\n"
        end
        snippet = "<div class=\"main\">\n#{snippet}</div>"
        snippet
      end
    end
  end
end
