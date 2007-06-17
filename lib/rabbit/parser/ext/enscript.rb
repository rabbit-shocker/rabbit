begin
  require 'htree'
rescue LoadError
end

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
          enscript_highlight = `enscript --help-highlight`.scan(/^Name: (\w+)/)
        rescue Errno::ENOENT => ignored
        end
        enscript_highlight.flatten.each do |name|
          @@enscript_highlight[name.downcase] = name
        end

        module_function
        def available?(lang)
          @@enscript_highlight.has_key?(lang)
        end

        def highlight(text, lang, logger)
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
          unless defined?(HTree)
            logger.warn(_("enscript: can't find HTree library"))
            return nil
          end
          tree = HTree(extract_newline_around_pre(html))
          pre = find_element(tree, "pre")
          address = find_element(tree, "address")
          element = tree_to_rabbit(pre, logger)
          if element
            logger.info(address.extract_text.to_s)
            PreformattedBlock.new(element)
          else
            nil
          end
        end

        def extract_newline_around_pre(html)
          html = html.gsub(/<pre[^>]*>\n/i, '<pre\1>')
          html.gsub(/\n<\/pre>/i, '</pre>')
        end

        def find_element(tree, name)
          tree.find_element("{http://www.w3.org/1999/xhtml}#{name}")
        end

        def tree_to_rabbit(tree, logger)
          element = element_to_rabbit(tree, logger)
          return nil if element.nil?
          tree.each_child do |child|
            if child.text?
              element << Text.new(Escape.escape_meta_character(child.to_s))
            else
              child_element = tree_to_rabbit(child, logger)
              element << child_element unless child_element.nil?
            end
          end
          element
        end

        def element_to_rabbit(element, logger)
          case element.qualified_name
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
            logger.warn(format % element.qualified_name)
            nil
          end
        end
      end
    end
  end
end
