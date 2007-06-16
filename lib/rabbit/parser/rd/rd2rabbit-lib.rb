require "forwardable"

require "rabbit/element"
require 'rabbit/parser/ext/text'
require 'rabbit/parser/rd/visitor'
require "rabbit/parser/rd/ext/refer"
require "rabbit/parser/rd/ext/inline-verbatim"
require "rabbit/parser/rd/ext/block-verbatim"

module Rabbit
  module Parser
    class RD
      class RD2RabbitVisitor < Visitor
        extend Forwardable

        include ::RD::MethodParse
        include Element
        include Parser::Ext::Text

        SYSTEM_NAME = "RD2RabbitLVisitor"
        SYSTEM_VERSION = "0.0.2"
        VERSION = ::RD::Version.new_from_version_string(SYSTEM_NAME,
                                                        SYSTEM_VERSION)

        # must-have constants
        OUTPUT_SUFFIX = ""
        INCLUDE_SUFFIX = ["rabbit", "rb"]

        EXTENSIONS = {
          "refer" => Ext::Refer,
          "inline_verbatim" => Ext::InlineVerbatim,
          "block_verbatim" => Ext::BlockVerbatim,
        }

        def_delegators(:@canvas, :logger, :full_path, :tmp_dir_name)

        attr_reader :canvas
        def initialize(canvas)
          @canvas = canvas

          @slides = []
          @slide = nil
          @index = {}

          init_extensions
          super()
        end

        def visit(tree)
          prepare_labels(tree, "label-")
          prepare_footnotes(tree)
          super(tree)
        end

        def apply_to_DocumentElement(element, contents)
          target = nil
          display = false
          contents.each do |content|
            display = false if content.nil?
            case content
            when nil
              # ignore
            when Slide
              target = Body.new
              content << target
              @canvas << content
              display = true
            when TitleSlide
              target = content
              @canvas << content
              display = true
            else
              target << content if display
            end
          end
          burn_out_foot_texts
        end

        def apply_to_Headline(element, title)
          anchor = get_anchor(element)
          if element.level == 1
            if @slides.empty?
              @slide = TitleSlide.new(Title.new(title))
            else
              @slide = Slide.new(HeadLine.new(title))
            end
            @foot_texts << []
            @slides << @slide
            @slide
          else
            @slide = nil
            nil
          end
        end

        def apply_to_Include(element)
          paths = element.tree.include_paths
          fname = search_file(element.filename, paths, @include_suffix)
          if fname
            File.open(fname) do |f|
              instance_eval(f.read, fname, 0)
            end
          end
        end

        def apply_to_TextBlock(element, content)
          Paragraph.new(content)
        end

        def apply_to_Verbatim(element)
          content = []
          element.each_line do |line|
            content << line
          end
          content_str = content.join("")
          /\A#\s*([^\n]+)(?:\n)?(?m:(.*)?)\z/ =~ content_str
          apply_to_extension("block_verbatim", $1, $2.to_s, content_str)
        end

        def apply_to_ItemList(element, items)
          apply_to_List(element, items, ItemList)
        end

        def apply_to_EnumList(element, items)
          i = 1
          apply_to_List(element, items, EnumList) do |list, item|
            item.order = i
            i += 1
          end
        end

        def apply_to_DescList(element, items)
          apply_to_List(element, items, DescriptionList)
        end

        def apply_to_MethodList(element, items)
          apply_to_List(element, items, MethodList)
        end

        def apply_to_List(element, items, klass)
          list = klass.new()
          items.each do |item|
            list << item
            if block_given?
              yield(list, item)
            end
          end
          list
        end

        def apply_to_ItemListItem(element, content)
          apply_to_ListItem(element, content, ItemListItem.new)
        end

        def apply_to_EnumListItem(element, content)
          apply_to_ListItem(element, content, EnumListItem.new)
        end

        def apply_to_DescListItem(element, term, description)
          desc_term = DescriptionTerm.new(term)
          desc_content = DescriptionContent.new
          apply_to_ListItem(element, description, desc_content)
          DescriptionListItem.new(desc_term, desc_content)
        end

        def apply_to_MethodListItem(element, term, description)
          method_term = parse_method(term)
          method_description = MethodDescription.new
          apply_to_ListItem(element, description, method_description)
          MethodListItem.new(method_term, method_description)
        end

        def apply_to_ListItem(element, contents, item)
          contents.each do |content|
            item << content
          end
          item
        end

        def apply_to_StringElement(element)
          content = element.content.gsub(/\n\s*/, '')
          Text.new(apply_to_String(content))
        end

        def apply_to_Emphasis(element, content)
          Emphasis.new(content)
        end

        def apply_to_Code(element, content)
          Code.new(content)
        end

        def apply_to_Var(element, content)
          Variable.new(content)
        end

        def apply_to_Keyboard(element, content)
          Keyboard.new(content)
        end

        def apply_to_Index(element, content)
          Index.new(content)
        end

        def apply_to_Reference_with_RDLabel(element, content)
          source = content.collect{|elem| elem.text}
          apply_to_extension("refer", element.label, source, content)
        end

        def apply_to_Reference_with_URL(element, content)
          ref = ReferText.new(content)
          ref.to = element.label.url
          ref
        end

        def apply_to_Footnote(element, content)
          if @slide.nil?
            Text.new("")
          else
            num = get_footnote_num(element)
            unless num
              raise ArgumentError, "[BUG?] #{element} is not registered."
            end
            add_foot_text(num, content)
            Footnote.new(num)
          end
        end

        def apply_to_Foottext(element, content)
          num = get_footnote_num(element)
          raise ArgumentError, "[BUG] #{element} isn't registered." unless num
          FootText.new(num, content)
        end

        def apply_to_Verb(element)
          source = apply_to_String(element.content)
          content = element.content
          apply_to_extension("inline_verbatim", element.to_label,
                             source, content)
        end

        def apply_to_String(str)
          escape_meta_character(str)
        end

        def create_have_text_element(klass, content)
          raise "Why???" if content.size > 1
          klass.new(content.collect{|x| x.text}.join(""))
        end

        private
        def prepare_footnotes(tree)
          @footnotes = tree.find_all{|i| i.is_a? ::RD::Footnote}
          @foot_texts = []
        end

        def get_footnote_num(fn)
          unless fn.is_a?(::RD::Footnote)
            raise ArgumentError, "#{fn} must be Footnote."
          end
          i = @footnotes.index(fn)
          if i
            i + 1
          else
            nil
          end
        end

        def parse_method(method)
          klass, kind, method, args = ::RD::MethodParse.analize_method(method)

          term = MethodTerm.new

          if kind == :function
            klass = kind = nil
          else
            kind = ::RD::MethodParse.kind2str(kind)
          end

          case method
          when "self"
            klass, kind, method, args = ::RD::MethodParse.analize_method(args)

            term = make_pre_inited_method_term(klass, kind)
            term << MethodName.new(Text.new("self"))
            add_args_to_method_term(term, args)
            term
          when "[]"
            args.strip!
            args.sub!(/^\((.*)\)$/, '\\1')
            args.split(/,/)
            term = make_pre_inited_method_term(klass, kind)
            term << MethodName.new(Text.new("["))
            args.each do |arg|
              term << Variable.new(Text.new(arg))
            end
            term << MethodName.new(Text.new("] = "))
            term
          when "[]="
            args.tr!(' ', '')
            args.sub!(/^\((.*)\)$/, '\\1')
            ary = args.split(/,/)

            case ary.length
            when 1
              args = []
              val = 'val'
            when 2
              args, val = *ary
              args = [Variable.new(Text.new(args))]
            when 3
              args = ary[0, 2].inject([]) do |result, x|
                result + [Variable.new(Text.new(x)), Code.new(Text.new(", "))]
              end
              args.pop # removed too much ", "
              val = ary[2]
            end

            term = make_pre_inited_method_term(klass, kind)
            term << MethodName.new(Text.new("["))
            args.each do |arg|
              term << arg
            end
            term << MethodName.new(Text.new("] = "))
            term << Variable.new(Text.new(val))
            term
          else
            term = make_pre_inited_method_term(klass, kind)
            term << MethodName.new(Text.new(method))
            term.name = method
            add_args_to_method_term(term, args)
            term
          end
        end

        def make_pre_inited_method_term(klass, kind)
          term = MethodTerm.new
          term << ClassName.new(Text.new(klass)) if klass
          term << MethodKind.new(Text.new(kind)) if kind
          term
        end

        def add_args_to_method_term(term, args)
          first = true
          prev_index = 0
          args.scan(/(?:&#?)?\w+;?/) do |m|
            code = $PREMATCH[prev_index..-1]
            term << Code.new(Text.new(code)) unless code.empty?
            prev_index = $PREMATCH.size + m.size
            if /(?:&#?)\w+;/ =~ m
              term << Code.new(Text.new(m))
            else
              term << Variable.new(Text.new(m))
            end
          end
          term << Code.new(Text.new($POSTMATCH)) if $POSTMATCH
          term
        end

        def add_foot_text(num, foot_text)
          unless @footnotes[num - 1]
            raise ArgumentError, "[BUG] footnote ##{num} isn't here."
          end
          @foot_texts.last << [foot_text, num - 1]
        end

        def burn_out_foot_texts
          @slides.each do |slide|
            ftb = FootTextBlock.new
            current_foot_texts = @foot_texts.shift
            while ft_info = current_foot_texts.shift
              ft, num = ft_info
              ftb << apply_to_Foottext(@footnotes[num], ft)
            end
            slide << ftb
          end
        end
      end
    end
  end
end
