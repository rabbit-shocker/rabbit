require 'English'

require 'gtk2'

require 'rabbit/rabbit'
require 'rabbit/image'
require 'rabbit/theme/searcher'
require 'rabbit/theme-browser/tag'

module Rabbit
  class ThemeBrowser
    class Document
      include GetText
      
      attr_reader :view, :name
      
      def initialize(page)
        @page = page
        @name = nil
        @hovering = false
        @category_buffers = {}
        @theme_buffers = {}
        load_itemize_icon
        load_cursor
        init_gui
      end
      
      def change_buffer(name, type)
        @name = name
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
      
      def init_tags(buffer)
        Tag::INFOS.each do |name, properties|
          buffer.create_tag(name, properties)
        end
      end
      
      def load_itemize_icon
        rabbit_image_theme = Theme::Searcher.find_theme("rabbit-images")
        icon_file = Theme::Searcher.find_file("green-item.png",
                                              [rabbit_image_theme])
        loader = ImageLoader.new(icon_file)
        loader.resize(10, 10)
        @itemize_icon = loader.pixbuf
      end
      
      def load_cursor
        @hand_cursor = Gdk::Cursor.new(Gdk::Cursor::HAND2)
        @regular_cursor = Gdk::Cursor.new(Gdk::Cursor::XTERM)
      end
      
      def update_buffer(name, buffers)
        buffer = buffers[name]
        if buffer.nil?
          buffer = Gtk::TextBuffer.new
          init_tags(buffer)
          buffers[name] = buffer
          yield(buffer)
        end
        @view.buffer = buffer
      end
      
      def change_to_category_buffer(name)
        update_buffer(name, @category_buffers) do |buffer|
          iter = buffer.start_iter
          insert_heading(buffer, iter, _(name))
          @page.themes.each do |entry|
            if entry.category == name
              insert_item(buffer, iter) do |buffer, iter|
                insert_theme_link(buffer, iter, entry.name, _(entry.title))
              end
            end
          end
        end
      end
      
      def change_to_theme_buffer(name)
        update_buffer(name, @theme_buffers) do |buffer|
          entry = @page.themes.find {|entry| entry.name == name}
          iter = buffer.start_iter
          insert_entry(buffer, iter, entry)
        end
      end
      
      def insert_entry(buffer, iter, entry)
        insert_title(buffer, iter, entry)
        insert_name(buffer, iter, entry)
        insert_category(buffer, iter, entry)
        insert_abstract(buffer, iter, entry) if entry.abstract
        insert_description(buffer, iter, entry) if entry.description
        unless entry.dependencies.empty?
          insert_dependencies(buffer, iter, entry)
        end
        unless entry.parameters.empty?
          insert_parameters(buffer, iter, entry)
        end
      end
      
      def insert_title(buffer, iter, entry)
        insert_heading(buffer, iter, _(entry.title))
      end
      
      def insert_name(buffer, iter, entry)
        insert_item(buffer, iter) do |buffer, iter|
          buffer.insert(iter, _("Name: "))
          buffer.insert(iter, entry.name)
        end
      end
      
      def insert_category(buffer, iter, entry)
        insert_item(buffer, iter) do |buffer, iter|
          buffer.insert(iter, _("Category: "))
          insert_category_link(buffer, iter, entry.category)
        end
      end
      
      def insert_abstract(buffer, iter, entry)
        insert_item(buffer, iter) do |buffer, iter|
          buffer.insert(iter, _("Abstract: "))
          buffer.insert(iter, _(entry.abstract))
        end
      end
      
      def insert_description(buffer, iter, entry)
        insert_heading(buffer, iter, _("Description"), 2)
        buffer.insert(iter, "#{_(entry.description)}\n", "description")
      end
      
      def insert_dependencies(buffer, iter, entry)
        insert_heading(buffer, iter, _("Dependencies"), 2)
        entry.dependencies.each do |dependency|
          insert_item(buffer, iter) do |buffer, iter|
            e = @page.themes.find {|e| e.name == dependency}
            insert_theme_link(buffer, iter, e.name, _(e.title))
          end
        end
      end
      
      def insert_parameters(buffer, iter, entry)
        insert_heading(buffer, iter, _("Parameters"), 2)
        entry.parameters.each do |name, info|
          insert_parameter(buffer, iter, name, info)
        end
      end
      
      def insert_heading(buffer, iter, text, level=1)
        buffer.insert(iter, "#{text}\n", "heading#{level}")
      end
      
      def insert_link(buffer, iter, type, name, text=nil)
        text ||= _(name)
        start_offset = iter.offset
        buffer.insert(iter, text, "link")
        tag = buffer.create_tag("#{type}-link-#{name}", {})
        buffer.apply_tag(tag,
                         buffer.get_iter_at_offset(start_offset),
                         iter)
      end
      
      def insert_theme_link(buffer, iter, name, text)
        insert_link(buffer, iter, "theme", name, text)
      end
      
      def insert_category_link(buffer, iter, name, text=nil)
        insert_link(buffer, iter, "category", name, text)
      end
      
      def insert_item(buffer, iter, text=nil)
        start_offset = iter.offset
        buffer.insert(iter, @itemize_icon)
        buffer.insert(iter, " ")
        item_start_offset = iter.offset
        if text.nil?
          yield(buffer, iter)
        else
          buffer.insert(iter, text)
        end
        buffer.apply_tag("item-content",
                         buffer.get_iter_at_offset(item_start_offset),
                         iter)
        buffer.apply_tag("item",
                         buffer.get_iter_at_offset(start_offset),
                         iter)
        buffer.insert(iter, "\n")
      end
      
      def insert_parameter(buffer, iter, name, info)
        start_offset = iter.offset
        buffer.insert(iter, name, "parameter-name")
        buffer.insert(iter, " (")
        buffer.insert(iter, _("default: "))
        buffer.insert(iter, info[:default], "parameter-default")
        buffer.insert(iter, ")\n")
        buffer.insert(iter, _(info[:description]), "parameter-description")
        buffer.insert(iter, "\n")
        buffer.apply_tag("parameter",
                         buffer.get_iter_at_offset(start_offset),
                         iter)
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
          name = get_name_from_link_tag(tag)
          if name
            @page.change_tree(name)
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
      
      def get_name_from_link_tag(tag)
        if /^(theme|category)-link-/ =~ tag.name
          $POSTMATCH
        end
      end
    end
  end
end
