require 'nokogiri'

require 'rabbit/utils'
require 'rabbit/parser/ext/escape'

module Rabbit
  module Parser
    module Ext
      module Enscript
        include Element
        include GetText

        @@enscript_highlight = {}
        enscript_highlight = []
        begin
          enscript_highlight = `enscript --help-highlight`
          if enscript_highlight.respond_to?(:encode)
            enscript_highlight = enscript_highlight.encode("UTF-8", "ISO-8859-1")
          end
          enscript_highlight = enscript_highlight.scan(/^Name: (\w+)/)
        rescue Errno::ENOENT => ignored
        end
        enscript_highlight.flatten.each do |name|
          @@enscript_highlight[name.downcase] = name
        end

        module_function
        def check_availability(lang, logger)
          if @@enscript_highlight.has_key?(lang)
            true
          else
            logger.warn(_("enscript: unsupported language: %s") % lang)
            false
          end
        end

        def highlight(lang, text, logger)
          src_file = Tempfile.new("rabbit-enscript")
          src_file.open
          src_file.print(text)
          src_file.close
          html_file = Tempfile.new("rabbit-enscript-html")
          args = [
                  "--quiet", "--color", "--language=html",
                  "--highlight=#{@@enscript_highlight[lang]}",
                  "--output=#{html_file.path}",
                  src_file.path,
                 ]
          if SystemRunner.run("enscript", *args)
            html_file.open
            html_to_rabbit(html_file.read, logger)
          else
            nil
          end
        end

        def html_to_rabbit(html, logger)
          node = Nokogiri::HTML(extract_newline_around_pre(html))
          pre = find_element(node, "pre")
          address = find_element(node, "address")
          element = node_to_rabbit(pre, logger)
          if element
            logger.info(address.text) if address
            PreformattedBlock.new(element)
          else
            nil
          end
        end

        def extract_newline_around_pre(html)
          html = html.gsub(/<pre[^>]*>\n/i, '<pre\1>')
          html.gsub(/\n<\/pre>/i, '</pre>')
        end

        def find_element(node, name)
          node.css(name)[0]
        end

        def node_to_rabbit(node, logger)
          element = element_to_rabbit(node, logger)
          return nil if element.nil?
          node.children.each do |child|
            if child.text?
              element << Text.new(Escape.escape_meta_character(child.to_s))
            else
              child_element = node_to_rabbit(child, logger)
              element << child_element unless child_element.nil?
            end
          end
          element
        end

        def element_to_rabbit(element, logger)
          case element.name
          when "pre"
            PreformattedText.new
          when "b"
            Keyword.new
          when "i"
            Element::Comment.new
          when "font"
            text = TextContainer.new
            color = element.get_attribute("color").to_s
            text.add_default_prop("foreground", color)
            text
          else
            format = _("enscript: unsupported element name: %s")
            logger.warn(format % element.name)
            nil
          end
        end
      end
    end
  end
end
