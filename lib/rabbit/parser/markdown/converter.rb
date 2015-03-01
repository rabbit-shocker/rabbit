require "English"

require "rabbit/parser/pause-support"
require "rabbit/parser/ext/blockdiag"
require "rabbit/parser/ext/coderay"
require "rabbit/parser/ext/escape"
require "rabbit/parser/ext/image"
require "rabbit/parser/ext/tex"

module Rabbit
  module Parser
    class Markdown
      class Converter
        include Element
        include PauseSupport

        def initialize(canvas)
          @canvas = canvas
          @slides = []
          @slide = nil
          @slide_property_mode = false
        end

        def convert(element, context=nil)
          method_name = "convert_#{element.type}"
          method_name << "_#{context}" if context
          __send__(method_name, element)
        end

        private
        def convert_container(element, context=nil)
          elements = []
          element.children.each do |child|
            element = convert(child, context)
            case element
            when nil, :no_element
              # ignore
            else
              elements << element
            end
          end
          elements
        end

        def convert_root(element)
          target = nil
          mode = :ignore
          convert_container(element).each do |content|
            case content
            when :no_element
              next
            when nil
              mode = :ignore
            when Element::Slide
              target = content.body
              @canvas << content
              mode = :display
            when TitleSlide
              target = content
              @canvas << content
              mode = :display
            when SlidePropertySetter, NoteSetter
              target = content
              mode = :property
            else
              case mode
              when :display
                target << content
              when :property
                target.apply(content)
              end
            end
          end
          burn_out_pause_targets
        end

        def convert_header(element)
          slide, @slide = @slide, nil
          contents = convert_container(element)
          case element.options[:level]
          when 1
            if @slides.empty?
              @slide = TitleSlide.new(Title.new(contents))
            else
              @slide = Element::Slide.new(HeadLine.new(contents))
              @slide << Body.new
            end
            @slides << @slide
            @slide
          when 2
            if /\Anote\z/i =~ contents.first.text
              NoteSetter.new(@slides.last)
            else
              SlidePropertySetter.new(@slides.last)
            end
          else
            nil
          end
        end

        def text(content)
          Text.new(Parser::Ext::Escape.escape_meta_character(content))
        end

        def convert_text(element)
          text(element.value)
        end

        def convert_blank(element)
          :no_element
        end

        def convert_dl(element)
          list = DescriptionList.new
          term = nil
          content = nil
          convert_container(element).each do |item|
            case item
            when DescriptionTerm
              list << DescriptionListItem.new(term, content) if term
              term = item
            when DescriptionContent
              content = item
            end
          end
          list << DescriptionListItem.new(term, content) if term
          list
        end

        def create_paragraph(elements)
          paragraph = Paragraph.new(elements)
          register_pause(paragraph) if paragraph.have_wait_tag?
          paragraph
        end

        def convert_dt(element)
          DescriptionTerm.new(create_paragraph(convert_container(element)))
        end

        def convert_dd(element)
          DescriptionContent.new(convert_container(element))
        end

        def convert_p(element)
          child_types = element.children.collect {|child| child.type}
          if child_types == [:img]
            convert_container(element)[0]
          else
            if child_types.include?(:img)
              raise ParseError,
                      _("multiple ![alt]{image} in a paragraph isn't supported.")
            end
            if element.options[:transparent] and child_types == [:text]
              element.children.first.value.chomp!
            end
            create_paragraph(convert_container(element))
          end
        end

        def create_list(list_class, contents)
          list = list_class.new
          contents.each do |content|
            list << content
            if block_given?
              yield(list, content)
            end
          end
          list
        end

        def create_list_item(list_item_class, contents)
          list_item = list_item_class.new(contents)

          waited_paragraphs = list_item.elements.find_all do |element|
            element.is_a?(Paragraph) and element.have_wait_tag?
          end
          unless waited_paragraphs.empty?
            waited_paragraphs.each do |paragraph|
              paragraph.default_visible = true
              paragraph.clear_theme
              unregister_pause(paragraph)
            end

            list_item.default_visible = false
            list_item.clear_theme
            register_pause(list_item)
          end

          list_item
        end

        def convert_ul(element)
          create_list(ItemList, convert_container(element, "ul"))
        end

        def convert_li_ul(element)
          create_list_item(ItemListItem, convert_container(element))
        end

        def convert_ol(element)
          i = 1
          create_list(EnumList, convert_container(element, "ol")) do |list, item|
            item.order = i
            i += 1
          end
        end

        def convert_li_ol(element)
          create_list_item(EnumListItem, convert_container(element))
        end

        def convert_smart_quote(element)
          Text.new(Parser::Ext::Entity::TABLE[element.value.to_s])
        end

        def convert_typographic_sym(element)
          Text.new(Parser::Ext::Entity::TABLE[element.value.to_s])
        end

        def convert_table(element)
          table = Table.new
          convert_container(element).each do |item|
            table << item
          end
          table
        end

        def convert_thead(element)
          in_table_header do
            TableHead.new(convert_container(element))
          end
        end

        def in_table_header
          in_table_header, @in_table_header = @in_table_header, true
          yield
        ensure
          @in_table_header = in_table_header
        end

        def convert_tbody(element)
          TableBody.new(convert_container(element))
        end

        def convert_tr(element)
          TableRow.new(convert_container(element))
        end

        def convert_td(element)
          if @in_table_header
            header = TableHeader.new(convert_container(element))
            def header.default_align
              Pango::Layout::ALIGN_CENTER
            end
            header
          else
            TableCell.new(convert_container(element))
          end
        end

        def convert_comment(element)
          :no_element
        end

        def convert_codeblock(element)
          content = element.value.chomp
          language = detect_codeblock_language(element)
          if language
            converted = convert_codeblock_language(element, language, content)
          end
          converted || PreformattedBlock.new(PreformattedText.new(text(content)))
        end

        def detect_codeblock_language(element)
          lang = element.attr["lang"]
          return lang if lang

          language = element.attr["language"]
          return language if language

          klass = element.attr["class"]
          if klass and /\Alanguage-/ =~ klass
            return $POSTMATCH
          end

          nil
        end

        def convert_codeblock_language(element, language, content)
          case language
          when "blockdiag"
            args = [@canvas, content]
            Ext::Image.make_image_from_file(*args) do |src_file_path|
              [
                Ext::BlockDiag.make_image(src_file_path, element.attr, @canvas),
                element.attr,
              ]
            end
          else
            logger = @canvas.logger
            Ext::CodeRay.highlight(language, content, logger)
          end
        end

        def convert_blockquote(element)
          BlockQuote.new(convert_container(element))
        end

        def convert_img(element)
          options = element.attr.dup
          uri = options.delete("src")
          title = options.delete("title")
          alt = options.delete("alt")
          caption = title || alt
          options["caption"] ||= caption if caption
          if options["align"] == "right"
            body = @slides.last.body
            if body["background-image"]
              raise ParseError,
                    _("multiple ![]('XXX.png'){:align='right'} " + \
                      "isn't supported.")
            end
            body["background-image"] = uri
            options.each do |name, value|
              name = name.to_s.gsub(/_/, "-")
              body["background-image-#{name}"] = value
            end
            :no_element
          else
            image = Ext::Image.make_image(@canvas, uri, options)
            image || text(alt || src)
          end
        end

        def convert_em(element)
          Emphasis.new(Emphasis.new(convert_container(element)))
        end

        def convert_strong(element)
          Emphasis.new(Emphasis.new(convert_container(element)))
        end

        def convert_math(element)
          args = [@canvas, element.value]
          Ext::Image.make_image_from_file(*args) do |src_file_path|
            [Ext::TeX.make_image_by_LaTeX(src_file_path, {}, @canvas), {}]
          end
        end

        def convert_a(element)
          ref = ReferText.new(convert_container(element))
          ref.to = element.attr["href"]
          ref
        end

        def convert_br(element)
          Text.new("\n")
        end

        def convert_codespan(element)
          Code.new(text(element.value))
        end

        def convert_wait(element)
          WaitTag.new
        end
      end
    end
  end
end
