require "forwardable"
require "gtk2"
require "rexml/text"

require "rabbit/rabbit"
require "rabbit/utils"

module Rabbit

  class Frame

    include ScreenInfo
    extend Forwardable

    FALLBACK_LIMIT = 250
    
    def_delegators(:@window, :icon, :icon=, :set_icon)
    def_delegators(:@window, :icon_list, :icon_list=, :set_icon_list)
    def_delegators(:@window, :iconify, :show, :hide, :visible?)
    def_delegators(:@window, :set_size_request, :resize)
    
    def_delegators(:@canvas, :apply_theme, :theme_name)
    def_delegators(:@canvas, :saved_image_type=, :saved_image_basename=)
    def_delegators(:@canvas, :save_as_image, :each_slide_pixbuf)
    def_delegators(:@canvas, :print, :filename=, :output_html=)
    def_delegators(:@canvas, :rss_base_uri=)
    
    attr_reader :window, :logger

    def initialize(logger, canvas)
      @logger = logger
      @canvas = canvas
    end

    def destroyed?
      @window.nil? or @window.destroyed?
    end
    
    def quit
      @window.destroy unless destroyed?
      @window = nil
      true
    end

    def width
      @window.size[0]
    end

    def height
      @window.size[1]
    end

    def parse_rd(source, callback=nil, &block)
      @canvas.parse_rd(source, callback, &block)
    end

    def fullscreen
      @fullscreen_toggled = false
      @fullscreen = true
      @window.fullscreen
      fallback_fullscreen
    end

    def unfullscreen
      @fullscreen_toggled = false
      @fullscreen = false
      @window.unfullscreen
      fallback_unfullscreen
    end
    
    def toggle_fullscreen
      if fullscreen?
        unfullscreen
      else
        fullscreen
      end
    end

    def fullscreen?
      if @window.respond_to?(:fullscreen?)
        @window.fullscreen?
      else
        @fullscreen
      end
    end

    def main_window?
      @main_window
    end
    
    def update_title(new_title)
      @window.title = Utils.unescape_title(new_title)
    end

    def init_gui(width, height, main_window, window_type=nil)
      init_window(width, height, window_type)
      @fullscreen_toggled = false
      @fullscreen = false
      @iconify = false
      @main_window = main_window
      @window.keep_above = true unless @main_window
      @window.show
      @canvas.post_init_gui
    end

    def fullscreen_available?
      true
    end

    def iconify_available?
      true
    end

    private
    def init_window(width, height, window_type=nil)
      window_type ||= Gtk::Window::TOPLEVEL
      @window = Gtk::Window.new(window_type)
      @window.set_default_size(width, height)
      @window.resize(width, height)
      @window.set_app_paintable(true)
      set_window_signal
      @canvas.attach_to(self, @window)
    end

    def set_window_signal
      set_window_signal_window_state_event
      set_window_signal_destroy
    end

    def set_window_signal_window_state_event
      @window.signal_connect("window_state_event") do |widget, event|
        if event.changed_mask.fullscreen?
          @fullscreen_toggled = true
          if fullscreen?
            @canvas.fullscreened
          else
            @canvas.unfullscreened
          end
          @window.present
        elsif event.changed_mask.iconified?
          if @iconify
            @iconify = false
          else
            @canvas.iconified
            @iconify = true
          end
        end
      end
    end

    def set_window_signal_destroy
      @window.signal_connect("destroy") do
        @canvas.destroy
        if main_window? and Gtk.main_level > 0
          Gtk.main_quit
        end
      end
    end

    def fallback_fullscreen
      @prev_width = @prev_height = nil
      @prev_x = @prev_y = nil
      Gtk.timeout_add(FALLBACK_LIMIT) do
        unless @fullscreen_toggled
          @prev_width, @prev_height = width, height
          @prev_x, @prev_y = @window.position
          @window.hide
          @window.set_size_request(screen_width, screen_height)
          @window.decorated = false
          @window.show
          @window.move(0, 0)
          @window.present
          @canvas.fullscreened
        end
        false
      end
    end

    def fallback_unfullscreen
      Gtk.timeout_add(FALLBACK_LIMIT) do
        if !@fullscreen_toggled and
            @prev_width and @prev_height and
            @prev_x and @prev_y
          @window.hide
          @window.set_size_request(@prev_width, @prev_height)
          @window.decorated = true
          @window.show
          @window.move(@prev_x, @prev_y)
          @window.present
          @canvas.unfullscreened
        end
        false
      end
    end
  end

  class NullFrame
    class << self
      def def_null_methods(*names)
        names.each do |name|
          define_method(name) {|*args|}
        end
      end
    end

    def_null_methods(:icon, :icon=, :set_icon)
    def_null_methods(:icon_list, :icon_list=, :set_icon_list)

    def_null_methods(:fullscreen?, :quit)

    def fullscreen_available?
      false
    end

    def iconify_available?
      false
    end
  end
  
  class EmbedFrame < Frame

    def update_title(new_title)
    end

    def fullscreen_available?
      false
    end

    def iconify_available?
      false
    end
    
    def init_gui(width, height, main_window, window_type=nil)
      @window = Gtk::EventBox.new
      @window.set_size_request(width, height)
      @canvas.attach_to(self, @window)
      @fullscreen_toggled = false
      @fullscreen = false
      @iconify = false
      @main_window = main_window
      @window.show
      @canvas.post_init_gui
    end
  end
end
