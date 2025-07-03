# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "forwardable"

require_relative "../../element"
require_relative "../pause-support"
require_relative "../ext/escape"
require_relative "visitor"
require_relative "ext/refer"
require_relative "ext/inline-verbatim"
require_relative "ext/block-verbatim"

module Rabbit
  module Parser
    class RD
      module NoURLMark
        def to_reference_content
          [::RD::StringElement.new(url)]
        end
      end
    end
  end
end

module RD
  class Reference
    class URL
      prepend Rabbit::Parser::RD::NoURLMark
    end
  end
end

module Rabbit
  module Parser
    class RD
      class RD2RabbitVisitor < Visitor
        extend Forwardable

        include ::RD::MethodParse
        include Element
        include PauseSupport

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

        def_delegators(:@canvas, :full_path, :tmp_dir_name)

        attr_reader :canvas
        attr_reader :progress
        def initialize(canvas, progress)
          @canvas = canvas
          @progress = progress

          @slides = []
          @slide = nil
          @slide_property_mode = false
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
          mode = :ignore
          contents.each do |content|
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
          burn_out_pause_targets
          burn_out_foot_texts
        end

        def apply_to_Headline(element, title)
          anchor = get_anchor(element)
          slide, @slide = @slide, nil
          case element.level
          when 1
            if @slides.empty?
              @slide = TitleSlide.new(Title.new(title))
            else
              @slide = Slide.new(HeadLine.new(title))
              @slide << Body.new
            end
            @foot_texts << []
            @slides << @slide
            @slide
          when 2
            if /\Anote\z/i =~ title.first.text
              NoteSetter.new(@slides.last)
            else
              SlidePropertySetter.new(@slides.last)
            end
          else
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
          paragraph = Paragraph.new(content)
          register_pause(paragraph) if paragraph.have_wait_tag?
          paragraph
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
          desc_term = DescriptionTerm.new(Paragraph.new(term))
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
          waited_paragraphs = item.elements.find_all do |element|
            element.is_a?(Paragraph) and element.have_wait_tag?
          end
          unless waited_paragraphs.empty?
            waited_paragraphs.each do |paragraph|
              paragraph.default_visible = true
              paragraph.clear_theme
              unregister_pause(paragraph)
            end

            item.default_visible = false
            item.clear_theme
            register_pause(item)
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
          Parser::Ext::Escape.escape_meta_character(str)
        end

        def create_have_text_element(klass, content)
          raise "Why???" if content.size > 1
          klass.new(content.collect{|x| x.text}.join(""))
        end

        def current_body
          @slide.body
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
