require 'nokogiri'

require 'rabbit/utils'
require 'rabbit/parser/ext/escape'

module Rabbit
  module Parser
    module Ext
      module Emacs
        include Element
        include GetText

        module_function
        def highlight(text, logger, mode_line=nil)
          src_file = Tempfile.new("rabbit-emacs")
          src_file.open
          src_file.print("#{mode_line}\n") if mode_line
          src_file.print(text)
          src_file.close
          html_file = Tempfile.new("rabbit-emacs-html")
          args = [
                  "--batch",
                  "--eval",
                  "(htmlize-file \"#{src_file.path}\" \"#{html_file.path}\")",
                 ]
          if SystemRunner.run("emacs", *args)
            html_file.open
            html_to_rabbit(html_file.read, mode_line, logger)
          else
            nil
          end
        end

        def html_to_rabbit(html, mode_line, logger)
          html = remove_newline_around_pre(html)
          html = remove_mode_line(html, mode_line)
          node = Nokogiri::HTML(html)
          pre = find_element(node, "pre")
          address = find_element(node, "address")
          element = node_to_rabbit(pre, logger)
          if element
            logger.info(address.text) if address
            element
          else
            nil
          end
        end

        def remove_newline_around_pre(html)
          html = html.gsub(/<pre([^>]*)>\n/i, '<pre\1>')
          html.gsub(/\n<\/pre>/i, '</pre>')
        end

        def remove_mode_line(html, mode_line)
          return html if mode_line.nil?
          html.gsub(/<pre([^>]*)>([^\n]+\n)/i) do
            attributes = $1
            mode_line_html = $2
            mode_line_text = mode_line_html.gsub(/<.+?>/, '')
            if mode_line_text.strip == mode_line.strip
              "<pre#{attributes}>"
            else
              "<pre#{attributes}>#{mode_line_html}"
            end
          end
        end

        def find_element(node, name)
          node.css(name)[0]
        end

        def node_to_rabbit(node, logger)
          case node.name
          when "pre"
            element = SyntaxHighlightingBlock.new
            node.children.each do |child|
              if child.text?
                element << text_to_rabbit(child.text, "plain")
              else
                child_element = node_to_rabbit(child, logger)
                element << child_element unless child_element.nil?
              end
            end
            element
          when "span"
            # p [node.text, node["class"]]
            text_to_rabbit(node.text, normalize_class_name(node["class"]))
          else
            format = _("emacs: unsupported element name: %s")
            logger.warn(format % element.name)
            nil
          end
        end

        def normalize_class_name(name)
          case name
          when "comment-delimiter"
            "comment"
          when "keyword"
            "reserved"
          when "variable-name"
            "variable"
          else
            name
          end
        end

        def text_to_rabbit(text, type)
          escaped_text = Escape.escape_meta_character(text)
          text_element = SyntaxHighlightingText.new(Text.new(escaped_text))
          CustomTag.new("syntax-#{type}", text_element)
        end
      end
    end
  end
end
