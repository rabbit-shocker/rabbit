require 'rabbit/element'
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

        attr_reader :canvas
        def initialize(canvas)
          @canvas = canvas
        end

        def reset
          @title_slide = false

          @slides = []
          @parent = nil
          @index = {}

          @enum_order_stack = []
          @list_type_stack = []

          @foot_texts = []
        end

        def finish
          @slides.each do |slide|
            @canvas << slide
          end
        end

        def container(_for=nil)
          []
        end


        #
        # Procedures
        #

        def headline(level, title)
          if level == 1
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

          item = Paragraph.new(item.flatten)
          type = @list_type_stack.last
          case type
          when "ul"
            @parent << ItemListItem.new(item)
          when "ol"
            list_item = EnumListItem.new(item)
            list_item.order = @enum_order_stack.last
            @enum_order_stack[-1] += 1
            @parent << list_item
          else
            unsupported_list_type
          end
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
          return unless @parent

          desc_term = DescriptionTerm.new(Paragraph.new(dt))
          desc_content = DescriptionContent.new(Paragraph.new(dd))
          @parent << DescriptionListItem.new(desc_term, desc_content)
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
          @parent << PreformattedBlock.new(PreformattedText.new(contents))
        end

        def paragraph(contents)
          @parent << Paragraph.new(contents.flatten)
        end

        def block_plugin(src)
          @parent << (evaluate_block_plugin(src) || text("{{#{src}}}"))
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
          Text.new(item) # or Emphasis.new(item)?
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
          include Element

          def initialize(output)
            @output = output
          end

          def image(source, props={})
            Ext::Image.make_image(@output.canvas, source, props)
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
