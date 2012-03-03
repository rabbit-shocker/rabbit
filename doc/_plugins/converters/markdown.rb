require 'jekyll/converters/markdown'

module Jekyll
  class MarkdownConverter
    alias_method :convert_without_container, :convert
    def convert(content)
      snippet = convert_without_container(content)
      snippet = "<section class=\"contents\">\n#{snippet}\n</section>"
      snippet = "<div id=\"main\">\n#{snippet}\n</div>"
      snippet
    end
  end
end
