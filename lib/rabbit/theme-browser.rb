require 'gtk2'

require 'rabbit/rabbit'
require 'rabbit/image'
require 'rabbit/theme/searcher'
require 'rabbit/theme-browser/page'

module Rabbit
  class ThemeBrowser
    include GetText
    
    attr_reader :themes
    
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
        page = Page.new(self)
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
      @themes = Theme::Searcher.collect_theme
    end
  end
end
