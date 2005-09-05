require 'htree'

require 'rabbit/ext/block-verbatim'

module Rabbit
  module Ext
    module Enscript

      include Element
      include GetText
      
      @@enscript_highlight = `enscript --help-highlight`.scan(/^Name: (\w+)/)
      @@enscript_highlight.flatten!

      def enscript_block(label, lang, source, content, visitor)
        src, prop = parse_source(source)
        default_result = default_ext_block_verbatim(label, src, src, visitor)
        unless @@enscript_highlight.include?(lang)
          format = _("enscript: unsupported language: %s")
          visitor.logger.warn(format % lang)
          return default_result
        end
        src_file = Tempfile.new("rabbit-enscript")
        src_file.open
        src_file.print(src)
        src_file.close
        html_file = Tempfile.new("rabbit-enscript-html")
        args = [
          "--color", "--language=html",
          "--highlight=#{lang}", "--output=#{html_file.path}",
          src_file.path,
        ]
        if run("enscript", *args)
          html_file.open
          element = enscript_html_to_rabbit(html_file.read, visitor)
          element || default_result
        else
          default_result
        end
      end
      
      private
      def enscript_html_to_rabbit(html, visitor)
        tree = HTree(enscript_extract_newline_around_pre(html))
        pre = enscript_get_element(tree, "pre")
        address = enscript_get_element(tree, "address")
        element = enscript_tree_to_rabbit_element(pre, visitor)
        if element
          visitor.logger.info(address.extract_text.to_s)
          PreformattedBlock.new(PreformattedText.new(element))
        else
          nil
        end
      end

      def enscript_extract_newline_around_pre(html)
        html = html.gsub(/<pre[^>]*>\n/i, '<pre\1>')
        html.gsub(/\n<\/pre>/i, '</pre>')
      end

      def enscript_get_element(tree, name)
        tree.find_element("{http://www.w3.org/1999/xhtml}#{name}")
      end

      def enscript_tree_to_rabbit_element(tree, visitor)
        element = enscript_element_to_rabbit_element(tree, visitor)
        return nil if element.nil?
        tree.each_child do |child|
          if child.text?
            element << Text.new(visitor.apply_to_String(child.to_s))
          else
            child_element = enscript_tree_to_rabbit_element(child, visitor)
            element << child_element unless child_element.nil?
          end
        end
        element
      end
        
      def enscript_element_to_rabbit_element(element, visitor)
        case element.qualified_name
        when "pre"
          PreformattedText.new
        when "b"
          Keyword.new
        when "i"
          Comment.new
        when "font"
          ColoredText.new(element.get_attribute("color").to_s)
        else
          format = _("enscript: unsupported element name: %s")
          visitor.logger.warn(format % element.qualified_name)
          nil
        end
      end
      
      module_function
      def available?
        not @@enscript_highlight.empty?
      end
    end
  end
end
