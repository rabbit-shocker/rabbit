require 'gtk2'

require 'rabbit/rabbit'
require 'rabbit/image'
require 'rabbit/theme/searcher'

module Rabbit
  module Theme
    class Browser
      include GetText
      
      TREE_MODEL = [
        [:name, String],
        [:localized_name, String],
        [:type, String],
      ]
      
      TAG_INFOS = [
        [
          "heading",
          {
            "weight" => Pango::FontDescription::WEIGHT_BOLD,
            "pixels-above-lines" => 5,
            "pixels-below-lines" => 10,
            "left-margin" => 5,
            "size" => 18 * Pango::SCALE,
          }
        ],
        [
          "item",
          {
            "indent" => 15,
            "pixels-above-lines" => 2,
            "pixels-below-lines" => 2,
          }
        ],
        [
          "item-content",
          {
          }
        ],
        [
          "theme-link",
          {
            "foreground" => "blue",
            "underline" => Pango::AttrUnderline::SINGLE,
          }
        ],
      ]
      
      def initialize(locales=nil)
        @locales = locales
        @themes = Searcher.collect_theme
        @category_buffers = {}
        @theme_buffers = {}
        load_itemize_icon
        init_gui
        load_themes
      end
      
      def set_window_size(width, height)
        @window.set_size_request(width, height)
        @hpaned.position = width * 0.25
      end
      
      def run
        @window.show_all
      end
      
      private
      def init_gui
        init_window
        init_body
      end
      
      def init_window
        @window = Gtk::Window.new
        @window.signal_connect("destroy") do
          Gtk.main_quit
        end
        @window.signal_connect("key_press_event") do |widget, event|
          if event.state.control_mask? and event.keyval == Gdk::Keyval::GDK_q
            @window.destroy
          end
        end
        @vbox = Gtk::VBox.new
        @window.add(@vbox)
      end
      
      def init_body
        @hpaned = Gtk::HPaned.new
        @vbox.pack_end(@hpaned)
        init_document_tree
        init_document_view
      end
      
      def init_document_tree
        @tree = Gtk::TreeView.new
        scrolled_window = wrap_by_scrolled_window(@tree)
        @hpaned.add1(scrolled_window)
        model_types = TREE_MODEL.collect {|key, type| type}
        model = Gtk::TreeStore.new(*model_types)
        @tree.set_model(model)
        init_tree_columns
      end
      
      def init_tree_columns
        renderer = Gtk::CellRendererText.new
        @tree.insert_column(-1, _("Theme"), renderer,
                            "text" => column(:localized_name))
        @tree.selection.signal_connect("changed") do |selection|
          iter = selection.selected
          theme_changed(iter) if iter
        end
      end
      
      def theme_changed(iter)
        name = iter.get_value(column(:name))
        type = iter.get_value(column(:type))
        __send__("change_to_#{type}_buffer", name)
      end
      
      def init_document_view
        @view = Gtk::TextView.new
        scrolled_window = wrap_by_scrolled_window(@view)
        @hpaned.add2(scrolled_window)
        @view.wrap_mode = Gtk::TextTag::WRAP_WORD
        @view.editable = false
        @view.signal_connect("event-after") do |*args|
          view_event_after(*args)
        end
      end
      
      def column(key)
        TREE_MODEL.index(TREE_MODEL.assoc(key))
      end

      def load_itemize_icon
        rabbit_image_theme = Searcher.find_theme("rabbit-images")
        icon_file = Searcher.find_file("green-item.png", [rabbit_image_theme])
        loader = ImageLoader.new(icon_file)
        loader.resize(10, 10)
        @itemize_icon = loader.pixbuf
      end
      
      def load_themes
        @themes = Searcher.collect_theme do |entry|
          Utils.process_pending_events
        end
        
        model = @tree.model
        categories = {}
        @themes.each do |entry|
          category = entry.category
          iter = categories[category]
          if iter.nil?
            iter = model.append(nil)
            categories[category] = iter
            iter[column(:type)] = "category"
            iter[column(:name)] = category
            iter[column(:localized_name)] = _(category)
          end
          child_iter = model.append(iter)
          child_iter[column(:type)] = "theme"
          child_iter[column(:name)] = entry.name
          child_iter[column(:localized_name)] = _(entry.name)
          Utils.process_pending_events
        end
        @tree.expand_all
      end
      
      def init_tags(buffer)
        TAG_INFOS.each do |name, properties|
          buffer.create_tag(name, properties)
        end
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
          @themes.each do |entry|
            if entry.category == name
              insert_item(buffer, iter) do |buffer, iter|
                insert_theme_link(buffer, iter, _(entry.name), entry.name)
              end
            end
          end
        end
      end

      def change_to_theme_buffer(name)
        update_buffer(name, @theme_buffers) do |buffer|
          entry = @themes.find {|entry| entry.name == name}
          iter = buffer.start_iter
          insert_heading(buffer, iter, _(entry.name))
          buffer.insert(iter, _(entry.category))
        end
      end

      def insert_heading(buffer, iter, text)
        buffer.insert(iter, "#{text}\n", "heading")
      end

      def insert_theme_link(buffer, iter, text, name)
        start_offset = iter.offset
        buffer.insert(iter, text, "theme-link")
        tag = buffer.create_tag("theme-link-#{name}", {})
        buffer.apply_tag(tag,
                         buffer.get_iter_at_offset(start_offset),
                         iter)
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

      def wrap_by_scrolled_window(widget)
        scrolled_window = Gtk::ScrolledWindow.new
        scrolled_window.set_policy(Gtk::PolicyType::AUTOMATIC,
                                   Gtk::PolicyType::AUTOMATIC)
        scrolled_window.add(widget)
        scrolled_window
      end

      def view_event_after(view, event)
        unless event.kind_of?(Gdk::EventButton) and
            event.button == 1 and
            event.event_type == Gdk::Event::BUTTON_RELEASE
          return false
        end

        buffer = view.buffer
        range = buffer.selection_bounds
        if range and range[0].offset != range[1].offset
          return false
        end

        x, y = view.window_to_buffer_coords(Gtk::TextView::WINDOW_WIDGET,
                                            event.x, event.y)
        iter = view.get_iter_at_location(x, y)
        follow_if_link(view, iter)
      end

      def follow_if_link(view, iter)
        iter.tags.each do |tag|
          if /^theme-link-/ =~ tag.name
            name = $POSTMATCH
            name_column = column(:name)
            @tree.model.each do |model, path, iter|
              if name == iter.get_value(name_column)
                @tree.expand_to_path(path)
                @tree.selection.select_iter(iter)
                break
              end
            end
          end
        end
      end
    end
  end
end
