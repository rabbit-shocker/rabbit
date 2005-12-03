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
  end
end
