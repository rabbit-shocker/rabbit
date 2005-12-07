require 'English'
require 'forwardable'

require 'gtk2'
require "rd/rdfmt"

require 'rabbit/rabbit'
require 'rabbit/image'
require 'rabbit/theme/searcher'
require 'rabbit/theme-browser/rd2document-lib'

module Rabbit
  class ThemeBrowser
    class Document
      extend Forwardable
      include GetText

      @@tag_table = nil
      class << self
        def load_tag_table(themes)
          @@tag_table = Gtk::TextTagTable.new
          theme_infos = themes.collect do |entry|
            ["theme-link-#{entry.name}", {}]
          end
          [theme_infos, Tag::INFOS].each do |infos|
            infos.each do |name, properties|
              tag = Gtk::TextTag.new(name)
              properties.each do |key, value|
                tag.set_property(key, value)
              end
              @@tag_table.add(tag)
            end
          end
        end
      end
      
      def_delegators(:@page, :logger)

      attr_reader :view, :name, :type
      
      def initialize(page)
        @page = page
        @name = nil
        @type = nil
        @hovering = false
        @category_buffers = {}
        @theme_buffers = {}
        load_cursor
        init_gui
      end
      
      def change_buffer(name, type)
        @name, @type = name, type
        __send__("change_to_#{type}_buffer", name)
      end
      
      private
      def init_gui
        @view = Gtk::TextView.new
        @view.wrap_mode = Gtk::TextTag::WRAP_WORD
        @view.editable = false
        @view.signal_connect("event-after") do |_, *args|
          event_after(*args)
        end
        @view.signal_connect("motion-notify-event") do |_, *args|
          motion_notify_event(*args)
        end
      end
      
      def load_cursor
        @hand_cursor = Gdk::Cursor.new(Gdk::Cursor::HAND2)
        @regular_cursor = Gdk::Cursor.new(Gdk::Cursor::XTERM)
      end
      
      def update_buffer(name, buffers)
        buffer = buffers[name]
        if buffer.nil?
          buffer = Gtk::TextBuffer.new(@@tag_table)
          buffers[name] = buffer
          yield(buffer)
        end
        @view.buffer = buffer
      end

      def rd2document(buffer, iter, source)
        tree = RD::RDTree.new("=begin\n#{source}\n=end\n")
        visitor = RD2DocumentVisitor.new(buffer, iter, logger)
        visitor.visit(tree)
      end
      
      def change_to_category_buffer(name)
        update_buffer(name, @category_buffers) do |buffer|
          source = "= #{_(name)}\n"
          @page.themes.each do |entry|
            if entry.category == name
              source << "  * ((<#{_(entry.title)}|#{entry.name}>))\n"
            end
          end
          iter = buffer.start_iter
          rd2document(buffer, iter, source)
        end
      end
      
      def change_to_theme_buffer(name)
        update_buffer(name, @theme_buffers) do |buffer|
          entry = @page.themes.find {|entry| entry.name == name}
          iter = buffer.start_iter
          rd = entry.to_rd
          if entry.image_theme?
            rd << "== %s\n" % _("Images")
            entry.files.each do |name|
              rd << "\n"
              rd << "=== #{File.basename(name)}\n"
              rd << "\n"
              rd << "  # image\n"
              rd << "  # src = file://#{name}\n"
              rd << "  # keep_ratio = true\n"
              rd << "  # height = 100\n"
              rd << "\n"
            end
          end
          rd2document(buffer, iter, rd)
        end
      end
      
      def event_after(event)
        unless event.kind_of?(Gdk::EventButton) and
            event.button == 1 and
            event.event_type == Gdk::Event::BUTTON_RELEASE
          return false
        end
        
        buffer = @view.buffer
        range = buffer.selection_bounds
        if range and range[0].offset != range[1].offset
          return false
        end
        
        x, y = view.window_to_buffer_coords(Gtk::TextView::WINDOW_WIDGET,
                                            event.x, event.y)
        iter = @view.get_iter_at_location(x, y)
        follow_if_link(iter)
        false
      end
      
      def follow_if_link(iter)
        iter.tags.each do |tag|
          name, type = link_tag_info(tag)
          if name and type
            @page.change_tree(name, type)
            break
          end
        end
      end
      
      def set_cursor_if_appropriate(x, y)
        buffer = @view.buffer
        iter = @view.get_iter_at_location(x, y)
        
        hovering = false
        
        iter.tags.each do |tag|
          if link_tag?(tag)
            hovering = true
            break
          end
        end
        
        if hovering != @hovering
          @hovering = hovering
          window = @view.get_window(Gtk::TextView::WINDOW_TEXT)
          if @hovering
            cursor = @hand_cursor
          else
            cursor = @regular_cursor
          end
          window.cursor = cursor
        end
      end
      
      def motion_notify_event(event)
        x, y = @view.window_to_buffer_coords(Gtk::TextView::WINDOW_WIDGET,
                                             event.x, event.y)
        set_cursor_if_appropriate(x, y)
        @view.window.pointer
        false
      end
      
      def link_tag?(tag)
        /-?link-?/ =~ tag.name
      end
      
      def link_tag_info(tag)
        if /^(theme|category)-link-/ =~ tag.name
          [$POSTMATCH, $1]
        end
      end
    end
  end
end
