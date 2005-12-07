require 'gtk2'

require 'rabbit/rabbit'
require 'rabbit/theme-browser/tree'
require 'rabbit/theme-browser/document'

module Rabbit
  class ThemeBrowser
    class Page
      attr_reader :widget
        
      def initialize(browser)
        @browser = browser
        @back_paths = []
        @forward_paths = []
        @forwarding = true
        init_gui
      end
        
      def change_document(name, type)
        prev_name = @document.name
        unless prev_name.nil?
          paths = @forwarding ? @back_paths : @forward_paths
          paths.push([prev_name, @document.type])
        end
        update_move_button
        @document.change_buffer(name, type)
      end

      def change_tree(name, type)
        @tree.select(name, type)
      end
        
      def themes
        @browser.themes
      end

      def default_size_changed(width, height)
        @hpaned.position = width * 0.25
      end
        
      def reload
        position = @hpaned.position
        name, type = @document.name, @document.type
        @widget.remove(@hpaned)
        @browser.load_themes
        
        Tag.reload_tag_infos
        init_view
        
        @hpaned.position = position
        @hpaned.show_all
        change_tree(name, type) if name and type
      end
      
      private
      def init_gui
        @widget = Gtk::VBox.new
        init_toolbar
        @widget.pack_start(@toolbar, false)
        init_view
      end

      def init_toolbar
        @toolbar = Gtk::Toolbar.new
        @toolbar.toolbar_style = Gtk::Toolbar::ICONS
        @back = @toolbar.append(Gtk::Stock::GO_BACK, _("Go back")) do
          @forwarding = false
          change_tree(*@back_paths.pop)
          @forwarding = true
        end
        @forward = @toolbar.append(Gtk::Stock::GO_FORWARD, _("Go forward")) do
          change_tree(*@forward_paths.pop)
        end
        @reload = @toolbar.append(Gtk::Stock::REFRESH, _("Reload")) do
          reload
        end
        update_move_button
      end

      def init_view
        @hpaned = Gtk::HPaned.new
        @widget.pack_start(@hpaned)
        @tree = Tree.new(self)
        @document = Document.new(self)
        @hpaned.add1(wrap_by_scrolled_window(@tree.view))
        @hpaned.add2(wrap_by_scrolled_window(@document.view))
      end

      def update_move_button
        @back.sensitive = !@back_paths.empty?
        @forward.sensitive = !@forward_paths.empty?
      end

      def wrap_by_scrolled_window(widget)
        scrolled_window = Gtk::ScrolledWindow.new
        scrolled_window.set_policy(Gtk::PolicyType::AUTOMATIC,
                                   Gtk::PolicyType::AUTOMATIC)
        scrolled_window.add(widget)
        scrolled_window
      end
    end
  end
end
