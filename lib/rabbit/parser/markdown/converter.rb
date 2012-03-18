module Rabbit
  module Parser
    class Markdown
      class Converter
        include Element

        def initialize(canvas)
          @canvas = canvas
          @slides = []
          @slide = nil
          @slide_property_mode = false
        end

        def convert(element)
          __send__("convert_#{element.type}", element)
        end

        private
        def convert_container(element)
          elements = []
          element.children.each do |child|
            element = convert(child)
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
          require 'pp'
          convert_container(element).each do |content|
            case content
            when :no_element
              next
            when nil
              mode = :ignore
            when Slide
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
        end

        def convert_header(element)
          slide, @slide = @slide, nil
          contents = convert_container(element)
          case element.options[:level]
          when 1
            if @slides.empty?
              @slide = TitleSlide.new(Title.new(contents))
            else
              @slide = Slide.new(HeadLine.new(contents))
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

        def convert_dt(element)
          DescriptionTerm.new(Paragraph.new(convert_container(element)))
        end

        def convert_dd(element)
          DescriptionContent.new(convert_container(element))
        end

        def convert_p(element)
          if element.children.collect {|child| child.type} == [:img]
            convert_container(element)[0]
          else
            if element.children.any? {|child| child.type == :img}
              raise ParseError,
                      _("multiple ![alt]{image} in a paragraph isn't supported.")
            else
              Paragraph.new(convert_container(element))
            end
          end
        end

        def convert_ul(element)
          ItemList.new(convert_container(element))
        end

        def convert_li(element)
          ItemListItem.new(convert_container(element))
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
          lang = element.attr["lang"]
          if lang
            highlighted = Ext::CodeRay.highlight(lang, content, @canvas.logger)
            return highlighted if highlighted
          end
          PreformattedBlock.new(PreformattedText.new(text(content)))
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
              name = name.to_s.gsub(/_/, '-')
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
          ref.to = element.attr['href']
          ref
        end
      end
    end
  end
end
