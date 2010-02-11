require 'rabbit/element'
require 'rabbit/parser/pause-support'
require 'rabbit/parser/ext/escape'
require 'rabbit/parser/ext/inline'
require 'rabbit/parser/ext/image'
require 'rabbit/parser/ext/enscript'
require 'rabbit/parser/ext/tex'
require 'rabbit/parser/ext/anthy'
require 'rabbit/parser/ext/entity'

module Rabbit
  module Parser
    class Wiki
      class RabbitOutput
        include Element
        include PauseSupport

        attr_reader :canvas
        def initialize(canvas)
          @canvas = canvas
        end

        def reset
          @title_slide = false

          @slides = []
          @parent = nil
          @index = {}

          @slide_property_mode = false
          @target_slide = nil

          @enum_order_stack = []
          @list_type_stack = []

          @foot_texts = []
        end

        def finish
          @slides.each do |slide|
            @canvas << slide
          end
          burn_out_pause_targets
        end

        def container(_for=nil)
          []
        end

        def current_body
          @slides.last.body
        end

        #
        # Procedures
        #

        def headline(level, title)
          @slide_property_mode = false
          case level
          when 1
            slide = nil
            if @slides.empty?
              @title_slide = true
              slide = TitleSlide.new(Title.new(title))
              @parent = slide
            else
              @title_slide = false
              slide = Slide.new(HeadLine.new(title))
              body = Body.new
              slide << body
              @parent = body
            end
            @foot_texts << []
            @slides << slide
          when 2
            @slide_property_mode = true
            @parent, @target_slide = nil, @slides.last
          else
            @parent = nil
          end
        end

        def hrule
          @canvas.logger.warn(_("horizontal rule is unsupported")) if @parent
        end

        def list_begin
        end

        def list_end
        end

        def list_open(type)
          @enum_order_stack << 1 if type == "ol"
          @list_type_stack << type
          return unless @parent

          case type
          when "ul"
            list = ItemList.new
          when "ol"
            list = EnumList.new
          else
            unsupported_list_type(type)
          end

          if @list_type_stack.size > 1
            @parent.elements.last << list
          else
            @parent << list
          end
          @parent = list
        end

        def list_close(type)
          @enum_order_stack.pop if type == "ol"
          @list_type_stack.pop
          return unless @parent

          @parent = @parent.parent
          @parent = @parent.parent unless @list_type_stack.empty?
        end

        def listitem_open
        end

        def listitem_close
        end

        def listitem(item)
          return unless @parent

          list_item = nil
          item = item.flatten
          paragraph = Paragraph.new(item)
          type = @list_type_stack.last
          case type
          when "ul"
            list_item = ItemListItem.new(paragraph)
            @parent << list_item
          when "ol"
            list_item = EnumListItem.new(paragraph)
            list_item.order = @enum_order_stack.last
            @enum_order_stack[-1] += 1
            @parent << list_item
          else
            unsupported_list_type
          end

          return unless paragraph.have_wait_tag?

          paragraph.default_visible = true
          paragraph.clear_theme
          register_pause(list_item)
        end

        def dlist_open
          return unless @parent

          list = DescriptionList.new
          list.parent = @parent
          @parent = list
        end

        def dlist_close
          return unless @parent

          list = @parent
          @parent = list.parent
          @parent << list
        end

        def dlist_item(dt, dd)
          if @slide_property_mode and @target_slide
            name = dt.collect {|element| element.text}.join
            value = dd.collect {|element| element.text}.join
            @target_slide[Parser.normalize_property_name(name)] = value.strip
          else
            return unless @parent

            desc_term = DescriptionTerm.new(Paragraph.new(dt.flatten))
            desc_content = DescriptionContent.new(Paragraph.new(dd.flatten))
            @parent << DescriptionListItem.new(desc_term, desc_content)
          end
        end

        def table_open
          return unless @parent

          @table = Table.new
          @parent << @table
          @parent = @table

          @table_head = TableHead.new
          @table_body = TableBody.new

          @have_table_header = false
          @have_table_body = false
        end

        def table_close
          return unless @parent

          @table << @table_head if @have_table_header
          @table << @table_body if @have_table_body

          @parent = @table.parent
        end

        def table_record_open
          return unless @parent

          @parent = TableRow.new
          @table_record_type = nil
        end

        def table_record_close
          return unless @parent

          @parent = @parent.parent
        end

        def table_head(item, rs, cs)
          return unless @parent

          @have_table_header = true
          @table_head << @parent if @parent.parent.nil?
          content = item.collect {|e| e.markuped_text}.join
          header = TableHeader.new(content)
          def header.default_align
            Pango::Layout::ALIGN_CENTER
          end
          @parent << header
        end

        def table_data(item, rs, cs)
          return unless @parent

          @have_table_body = true
          @table_body << @parent if @parent.parent.nil?
          content = item.collect {|e| e.markuped_text}.join
          @parent << TableCell.new(content)
        end

        def blockquote_open
          return unless @parent

          block_quote = BlockQuote.new
          @parent << block_quote
          @parent = block_quote
        end

        def blockquote_close
          return unless @parent

          @parent = @parent.parent
        end

        def block_preformatted(contents, info)
          return unless @parent

          lang = info ? info.downcase : nil
          result = nil

          if lang and Ext::Enscript.check_availability(lang, @canvas.logger)
            result = Ext::Enscript.highlight(lang, contents, @canvas.logger)
          end

          if result
            @parent << result
          else
            preformatted(text(contents))
          end
        end

        def preformatted(contents)
          return unless @parent

          @parent << PreformattedBlock.new(PreformattedText.new(contents))
        end

        def paragraph(contents)
          return unless @parent

          paragraph = Paragraph.new(contents.flatten)
          register_pause(paragraph) if paragraph.have_wait_tag?
          @parent << paragraph
        end

        def block_plugin(src)
          return unless @parent

          result = evaluate_block_plugin(src)
          return if result == :no_element
          @parent << (result || text("{{#{src}}}"))
        end


        #
        # Functions
        #

        def hyperlink(uri, title)
          ref = ReferText.new(title)
          ref.to = uri
          ref
        end

        # inline image is not supported yet...
        def image_hyperlink(uri, alt=nil)
          Ext::Image.make_image(@canvas, uri, "caption" => alt) || Text.new(uri)
        end

        def strong(item)
          Emphasis.new(Emphasis.new(item))
        end

        def em(item)
          Emphasis.new(item)
        end

        def del(item)
          DeletedText.new(item)
        end

        def text(str)
          Text.new(Ext::Escape.escape_meta_character(str))
        end

        def inline_plugin(src)
          evaluate_inline_plugin(src) || text("{{#{src}}}")
        end

        private
        def unsupported_list_type(type)
          @canvas.logger.warn(_("unsupported list type: %s") % type)
        end

        def evaluate_inline_plugin(src)
          InlinePlugin.new(self).instance_eval(src, "(inline plugin)")
        rescue
          @canvas.logger.warn($!)
          nil
        end

        def evaluate_block_plugin(src)
          BlockPlugin.new(self).instance_eval(src, "(block plugin)")
        rescue
          @canvas.logger.warn($!)
          nil
        end

        class InlinePlugin
          include Element
          include Parser::Ext::Inline
          include Parser::Ext::Entity

          def initialize(output)
            @private = Private.new(output)
          end

          def entity(entity, *rest)
            return nil unless TABLE.include?(entity)

            @private.pack(Text.new(TABLE[entity]), *rest)
          end
          alias_method :e, :entity

          def sub(text, *rest)
            super(@private.pack(text, *rest))
          end

          def sup(text, *rest)
            super(@private.pack(text, *rest))
          end

          def note(text, *rest)
            super(@private.pack(text, *rest))
          end

          def lang(lang, text)
            super(lang, @private.pack(text, *rest))
          end

          def br
            Text.new("\n")
          end

          def wait
            WaitTag.new
          end

          class Private
            include Element
            def initialize(output)
              @output = output
            end

            def pack(text, *rest)
              text = @output.text(text) unless text.is_a?(Element::Base)
              if rest.empty?
                text
              else
                TextContainer.new([text, *rest])
              end
            end
          end
        end

        class BlockPlugin
          include GetText
          include Element

          def initialize(output)
            @output = output
          end

          def image(source, props={})
            if props[:align].to_s == "right"
              body = @output.current_body
              if body["background-image"]
                raise ParseError,
                      _("multiple {{image, 'XXX.png', :align => :right}} " + \
                        "isn't supported.")
              end
              body["background-image"] = source
              props.each do |name, value|
                name = name.to_s.gsub(/_/, '-')
                value = value.to_s if name == "align"
                body["background-image-#{name}"] = value
              end
              :no_element
            else
              Ext::Image.make_image(@output.canvas, source, props)
            end
          end
          alias_method :img, :image

          def enscript(lang, source)
            logger = @output.canvas.logger
            if Ext::Enscript.check_availability(lang, logger)
              Ext::Enscript.highlight(lang, source, logger)
            else
              nil
            end
          end

          def anthy(source)
            unless Ext::Anthy.available?
              @output.canvas.logger.warn(_("Anthy isn't available"))
              return nil
            end

            converted_source = Ext::Anthy.hiragana_to_kanji(source)
            converted_source =
              Ext::Escape.escape_meta_character(converted_source)
            Paragraph.new(Text.new(converted_source))
          end

          def LaTeX(source, props={})
            args = [@output.canvas, source]
            Ext::Image.make_image_from_file(*args) do |src_file_path|
              args = [src_file_path, props, @output.canvas]
              [Ext::TeX.make_image_by_LaTeX(*args), props]
            end
          end
          alias_method :latex, :LaTeX

          def mimeTeX(source, props={})
            args = [@output.canvas, source]
            Ext::Image.make_image_from_file(*args) do |src_file_path|
              args = [src_file_path, props, @output.canvas]
              [Ext::TeX.make_image_by_mimeTeX(*args), props]
            end
          end
          alias_method :mimetex, :mimeTeX
        end
      end
    end
  end
end
