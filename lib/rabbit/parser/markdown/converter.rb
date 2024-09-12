# Copyright (C) 2012-2024  Sutou Kouhei <kou@cozmixng.org>
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

require "English"

require "rabbit/gettext"
require "rabbit/parser/pause-support"
require "rabbit/parser/ext/blockdiag"
require "rabbit/parser/ext/escape"
require "rabbit/parser/ext/image"
require "rabbit/parser/ext/inline"
require "rabbit/parser/ext/rouge"
require "rabbit/parser/ext/tex"

module Rabbit
  module Parser
    class Markdown
      class Converter
        include GetText
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
            if child_types.count(:img) > 1
              raise ParseError,
                    _("multiple ![alt]{image}s in a paragraph isn't supported.")
            end
            if child_types.include?(:img)
              format =
                _("![alt]{image} and other contents in a paragraph " \
                  "isn't supported: %{types}")
              raise ParseError, format % {types: child_types}
            end
            if element.options[:transparent] and child_types == [:text]
              element.children.first.value.chomp!
            end
            converted_children = apply_class(convert_container(element),
                                             element.attr["class"])
            create_paragraph(converted_children)
          end
        end

        def apply_class(children, klass)
          return children if klass.nil?
          classes = klass.split
          classes.inject(children) do |nested_children, klass|
            CustomTag.new(klass, nested_children)
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
          if waited_paragraphs.empty?
            list_item.default_visible = true
            list_item.clear_theme
          else
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
              Pango::Alignment::CENTER
            end
            header
          else
            TableCell.new(convert_container(element))
          end
        end

        def convert_hr(element)
          raise ParseError, _("horizontal rule isn't supported.")
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
            make_image_from_file(element, content) do |src_file, prop|
              Ext::BlockDiag.make_image(src_file.path, prop, @canvas)
            end
          when "mermaid"
            make_image_from_file(element,
                                 content,
                                 extension: ".mmd") do |src_file, prop|
              src_file
            end
          else
            logger = @canvas.logger
            Ext::Rouge.highlight(language, content, logger)
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
          image = Ext::Image.make_image(@canvas,
                                        uri,
                                        options,
                                        body: @slides.last.body)
          image || text(alt || src)
        end

        def convert_em(element)
          Emphasis.new(Emphasis.new(convert_container(element)))
        end

        def convert_strong(element)
          Emphasis.new(Emphasis.new(convert_container(element)))
        end

        def make_image_from_file(element, source, **options)
          Ext::Image.make_image_from_file(@canvas,
                                          source,
                                          body: @slides.last.body,
                                          **options) do |src_file|
            prop = element.attr
            image = yield(src_file, prop)
            [image, prop]
          end
        end

        def convert_math(element)
          make_image_from_file(element, element.value) do |src_file, prop|
            Ext::TeX.make_image_by_LaTeX(src_file.path, prop, @canvas)
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

        def convert_note(element)
          # TODO: Should we validate element.options[:category] == "span"?
          Ext::Inline.note(convert_container(element))
        end

        def convert_tag(element)
          name = element.attr["name"]
          if name.nil?
            raise ParseError, _("tag name is missing.")
          end
          if element.children.empty?
            CustomTag.new(name)
          else
            CustomTag.new(name, convert_container(element))
          end
        end

        def convert_strikethrough(element)
          DeletedText.new(text(element.value))
        end

        def convert_html_element(element)
          raise ParseError, _("HTML isn't supported.")
        end
      end
    end
  end
end
