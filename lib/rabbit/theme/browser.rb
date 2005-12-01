require 'gtk2'

require 'rabbit/rabbit'
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
					"category",
					{
						"weight" => Pango::FontDescription::WEIGHT_BOLD,
						"size" => 15 * Pango::SCALE,
					}
				]
			]
      
      def initialize(locales=nil)
        @locales = locales
        @themes = Rabbit::Theme::Searcher.collect_theme
				@buffers = {}
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
        scrolled_window = Gtk::ScrolledWindow.new
        scrolled_window.set_policy(Gtk::PolicyType::AUTOMATIC,
                                   Gtk::PolicyType::AUTOMATIC)
        scrolled_window.add(@tree)
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
				case iter.get_value(column(:type))
				when "theme"
					update_buffer(iter.get_value(column(:name)))
				end
			end

      def init_document_view
        @view = Gtk::TextView.new
        @hpaned.add2(@view)
        @view.wrap_mode = Gtk::TextTag::WRAP_WORD
      end

      def column(key)
        TREE_MODEL.index(TREE_MODEL.assoc(key))
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

			def update_buffer(name)
				buffer = @buffers[name]
				if buffer.nil?
					buffer = Gtk::TextBuffer.new
					init_tags(buffer)
					@buffers[name] = buffer
				end
				@view.buffer = buffer
				entry = @themes.find {|entry| entry.name == name}
				buffer.text = "#{_(entry.name)}\n#{_(entry.category)}"
			end
    end
  end
end
