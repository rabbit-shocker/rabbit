require 'English'

require 'gtk2'

require 'rabbit/rabbit'
require 'rabbit/image'
require 'rabbit/theme/searcher'

module Rabbit
  module Theme
    class Browser
      include GetText
      
      attr_reader :locales, :themes, :notebook
      
      def initialize(locales)
        @locales = locales
        @pages = {}
        load_themes
        init_gui
      end
      
      def set_window_size(width, height)
        @window.set_default_size(width, height)
        @pages.each_value do |page|
          page.default_size_changed(width, height)
        end
      end
      
      def run
        @window.show_all
        @notebook.page = @init_page_number
      end
      
      private
      def init_gui
        init_window
        current_locale = Locale.get
        @init_page_number = 0
        @locales.each_with_index do |locale, i|
          @init_page_number = i if /#{locale}/ =~ current_locale

          GetText.locale = current_locale
          locale_name_for_the_locale = _(locale)
          GetText.locale = locale
          page = Page.new(self, locale)
          @pages[locale] = page
          label = Gtk::Label.new(locale_name_for_the_locale)
          @notebook.append_page(page.widget, label)
        end
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
        init_notebook
      end
      
      def init_notebook
        @notebook = Gtk::Notebook.new
        @notebook.signal_connect("switch-page") do |book, page, n|
          @current_locale = @locales[n]
          GetText.locale = @current_locale
          false
        end
        @window.add(@notebook)
      end
      
      def load_themes
        @themes = Searcher.collect_theme
      end
      
      class Page
        attr_reader :widget
        
        def initialize(browser)
          @browser = browser
          init_gui
        end
        
        def change_document(name, type)
          @document.change_buffer(name, type)
        end

        def change_tree(name)
          @tree.select(name)
        end
        
        def themes
          @browser.themes
        end

        def default_size_changed(width, height)
          @hpaned.position = width * 0.25
        end
        
        private
        def init_gui
          @widget = Gtk::VBox.new
          @hpaned = Gtk::HPaned.new
          @widget.pack_end(@hpaned)
          @tree = Tree.new(self)
          @document = Document.new(self)
          @hpaned.add1(wrap_by_scrolled_window(@tree.view))
          @hpaned.add2(wrap_by_scrolled_window(@document.view))
        end
        
        def wrap_by_scrolled_window(widget)
          scrolled_window = Gtk::ScrolledWindow.new
          scrolled_window.set_policy(Gtk::PolicyType::AUTOMATIC,
                                     Gtk::PolicyType::AUTOMATIC)
          scrolled_window.add(widget)
          scrolled_window
        end
      end
      
      class Tree
        include GetText
        
        MODEL = [
          [:name, String],
          [:title, String],
          [:type, String],
        ]
        
        attr_reader :view
        
        def initialize(page)
          @page = page
          init_gui
        end

        def select(name)
          name_column = column(:name)
          @view.model.each do |model, path, iter|
            if name == iter.get_value(name_column)
              @view.expand_to_path(path)
              @view.selection.select_iter(iter)
              break
            end
          end
        end
        
        private
        def column(key)
          MODEL.index(MODEL.assoc(key))
        end
        
        def init_gui
          @view = Gtk::TreeView.new
          model_types = MODEL.collect {|key, type| type}
          model = Gtk::TreeStore.new(*model_types)
          @view.set_model(model)
          init_columns
          init_model
        end
        
        def init_columns
          renderer = Gtk::CellRendererText.new
          @view.insert_column(-1, _("Theme"), renderer,
                              "text" => column(:title))
          @view.selection.signal_connect("changed") do |selection|
            iter = selection.selected
            if iter
              name = iter.get_value(column(:name))
              type = iter.get_value(column(:type))
              @page.change_document(name, type)
            end
            false
          end
        end
        
        def init_model
          model = @view.model
          categories = {}
          @page.themes.each do |entry|
            category = entry.category
            iter = categories[category]
            if iter.nil?
              iter = model.append(nil)
              categories[category] = iter
              iter[column(:type)] = "category"
              iter[column(:name)] = category
              iter[column(:title)] = _(category)
            end
            child_iter = model.append(iter)
            child_iter[column(:type)] = "theme"
            child_iter[column(:name)] = entry.name
            child_iter[column(:title)] = _(entry.title)
          end
          # @view.expand_all
        end
      end
      
      class Document
        include GetText
        
        TAG_INFOS = [
          [
            "heading1",
            {
              "weight" => Pango::FontDescription::WEIGHT_BOLD,
              "pixels-above-lines" => 5,
              "pixels-below-lines" => 10,
              "left-margin" => 5,
              "size" => 18 * Pango::SCALE,
            }
          ],
          [
            "heading2",
            {
              "pixels-above-lines" => 5,
              "pixels-below-lines" => 5,
              "left-margin" => 5,
              "size" => 17 * Pango::SCALE,
            }
          ],
          [
            "item",
            {
              "left-margin" => 15,
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
            "link",
            {
              "foreground" => "blue",
              "underline" => Pango::AttrUnderline::SINGLE,
            }
          ],
          [
            "description",
            {
              "left-margin" => 20,
              "right-margin" => 20,
            }
          ],
          [
            "parameter",
            {
              "left-margin" => 20,
              "right-margin" => 20,
              "pixels-below-lines" => 4,
            }
          ],
          [
            "parameter-name",
            {
              "foreground" => "red",
              "family" => "Monospace",
            }
          ],
          [
            "parameter-default",
            {
              "foreground" => "gray",
              "family" => "Monospace",
              "style" => Pango::FontDescription::STYLE_ITALIC,
            }
          ],
          [
            "parameter-description",
            {
              "left-margin" => 40,
              "right-margin" => 40,
            }
          ],
        ]
        
        attr_reader :view
        
        def initialize(page)
          @page = page
          @hovering = false
          @category_buffers = {}
          @theme_buffers = {}
          load_itemize_icon
          load_cursor
          init_gui
        end

        def change_buffer(name, type)
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
          TAG_INFOS.each do |name, properties|
            buffer.create_tag(name, properties)
          end
        end
        
        def load_itemize_icon
          rabbit_image_theme = Searcher.find_theme("rabbit-images")
          icon_file = Searcher.find_file("green-item.png", [rabbit_image_theme])
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
            insert_heading(buffer, iter, _(entry.title))
            insert_item(buffer, iter) do |buffer, iter|
              buffer.insert(iter, _("Name: "))
              buffer.insert(iter, entry.name)
            end
            insert_item(buffer, iter) do |buffer, iter|
              buffer.insert(iter, _("Category: "))
              insert_category_link(buffer, iter, entry.category)
            end
            if entry.abstract
              insert_item(buffer, iter) do |buffer, iter|
                buffer.insert(iter, _("Abstract: "))
                buffer.insert(iter, _(entry.abstract))
              end
            end
            if entry.description
              insert_heading(buffer, iter, _("Description"), 2)
              buffer.insert(iter, "#{_(entry.description)}\n", "description")
            end
            unless entry.dependencies.empty?
              insert_heading(buffer, iter, _("Dependencies"), 2)
              entry.dependencies.each do |dependency|
                insert_item(buffer, iter) do |buffer, iter|
                  e = @page.themes.find {|e| e.name == dependency}
                  insert_theme_link(buffer, iter, e.name, _(e.title))
                end
              end
            end
            unless entry.parameters.empty?
              insert_heading(buffer, iter, _("Parameters"), 2)
              entry.parameters.each do |name, info|
                insert_parameter(buffer, iter, name, info)
              end
            end
          end
        end
        
        def insert_heading(buffer, iter, text, level=1)
          buffer.insert(iter, "#{text}\n", "heading#{level}")
        end
        
        def insert_theme_link(buffer, iter, name, text=nil)
          text ||= _(name)
          start_offset = iter.offset
          buffer.insert(iter, text, "link")
          tag = buffer.create_tag("theme-link-#{name}", {})
          buffer.apply_tag(tag,
                           buffer.get_iter_at_offset(start_offset),
                           iter)
        end
        
        def insert_category_link(buffer, iter, name, text=nil)
          text ||= _(name)
          start_offset = iter.offset
          buffer.insert(iter, text, "link")
          tag = buffer.create_tag("category-link-#{name}", {})
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
end
