require "forwardable"

require "rd/rdvisitor"
require "rd/version"

require 'rabbit/theme-browser/tag'

module Rabbit
  class ThemeBrowser
    class RD2DocumentVisitor < RD::RDVisitor

      @@itemize_icon = nil
      
      def initialize(buffer, iter)
        @buffer = buffer
        @iter = iter
        init_tags
        load_itemize_icon
        super()
      end

      def apply_to_DocumentElement(element, contents)
        tag("body") do
          insert_children(contents, "top-level-")
        end
      end

      def apply_to_Headline(element, contents)
        Proc.new do |*args|
          tag("heading#{element.level}") do
            insert_children(contents)
            insert("\n")
          end
        end
      end

      def apply_to_ItemList(element, items)
        Proc.new do |*args|
          items.each do |item|
            tag("item") do
              @buffer.insert(@iter, @@itemize_icon)
              @buffer.insert(@iter, " ")
              tag("item-content") do
                item.call
              end
            end
          end
        end
      end

      def apply_to_DescList(element, items)
        Proc.new do |*args|
          items.each do |item|
            tag("description") do
              item.call
            end
          end
        end
      end

      def apply_to_ItemListItem(element, contents)
        Proc.new do |*args|
          contents.each do |content|
            content.call
          end
        end
      end
  
      def apply_to_DescListItem(element, term, description)
        Proc.new do |*args|
          tag("description-term") do
            insert_children(term)
          end
          @buffer.insert(@iter, "\n")
          tag("description-content") do
            insert_children(description)
          end
        end
      end
  
      def apply_to_TextBlock(element, contents)
        Proc.new do |*args|
          prefix, = args
          tag("#{prefix}paragraph") do
            insert_children(contents)
            insert("\n")
          end
        end
      end

      def apply_to_StringElement(element)
        element.content.gsub(/\n\s*/, '')
      end

      def apply_to_Reference_with_RDLabel(element, contents)
        source = []
        contents.each do |elem|
          if elem.respond_to?(:call)
            elem.call
          else
            source << elem
          end
        end
        source = source.join("")
        Proc.new do |*args|
          insert_link(element.label.element_label, source)
        end
      end

      def apply_to_Code(element, contents)
        Proc.new do |*args|
          tag("code") do
            insert_children(contents)
          end
        end
      end
  
      private
      def init_tags
        Tag::INFOS.each do |name, properties|
          @buffer.create_tag(name, properties)
        end
      end

      def load_itemize_icon
        unless @@itemize_icon
          image_theme = Theme::Searcher.find_theme("rabbit-images", true)
          icon_file = Theme::Searcher.find_file("green-item.png",
                                                [image_theme])
          loader = ImageLoader.new(icon_file)
          loader.resize(10, 10)
          @@itemize_icon = loader.pixbuf
        end
      end

      def insert(text, *args)
        @buffer.insert(@iter, text, *args)
      end
      
      def insert_link(name, text=nil)
        text ||= _(name)
        start_offset = @iter.offset
        insert(text, "link")
        tag = @buffer.create_tag("theme-link-#{name}", {})
        @buffer.apply_tag(tag,
                          @buffer.get_iter_at_offset(start_offset),
                          @iter)
      end

      def tag(name)
        start_offset = @iter.offset
        yield
        @buffer.apply_tag(name,
                          @buffer.get_iter_at_offset(start_offset),
                          @iter)
      end

      def insert_children(children, *args)
        children.each do |child|
          if child.respond_to?(:call)
            child.call(*args)
          else
            insert(child)
          end
        end
      end
    end
  end
end
