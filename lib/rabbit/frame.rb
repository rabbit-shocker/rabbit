require "forwardable"
require "gtk2"
require "rexml/text"

require "rabbit/rabbit"

module Rabbit

  class Frame

    extend Forwardable

    FALLBACK_LIMIT = 100
    
    def_delegators(:@window, :icon, :icon=, :set_icon)
    def_delegators(:@window, :icon_list, :icon_list=, :set_icon_list)
    def_delegators(:@window, :iconify)
    
    def_delegators(:@canvas, :apply_theme, :theme_name)
    def_delegators(:@canvas, :saved_image_type=, :saved_image_basename=)
    def_delegators(:@canvas, :save_as_image, :each_page_pixbuf)
    def_delegators(:@canvas, :print, :filename=)
    
    attr_reader :window, :logger

    def initialize(logger, canvas)
      @logger = logger
      @canvas = canvas
    end

    def quit
      @window.destroy
      true
    end

    def width
      @window.size[0]
    end

    def height
      @window.size[1]
    end

    def parse_rd(source)
      @canvas.parse_rd(source)
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
      @window.title = unescape_title(new_title)
    end

    def init_gui(width, height, main_window)
      init_window(width, height)
      @fullscreen_toggled = false
      @fullscreen = false
      @iconify = false
      @main_window = main_window
      @window.keep_above = true unless @main_window
      @window.show_all
    end
    
    private
    def init_window(width, height)
      @window = Gtk::Window.new
      @window.set_default_size(width, height)
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

    def unescape_title(title)
      REXML::Text.unnormalize(title).gsub(/\r|\n/, '')
    end

    def fallback_fullscreen
      Gtk.timeout_add(FALLBACK_LIMIT) do
        unless @fullscreen_toggled
          @prev_width, @prev_height = width, height
          @prev_x, @prev_y = @window.position
          max_width, max_height = @window.root_window.size
          @window.hide
          @window.resize(max_width, max_height)
          @window.decorated = false
          @window.show_all
          @window.move(0, 0)
        end
        false
      end
    end

    def fallback_unfullscreen
      Gtk.timeout_add(FALLBACK_LIMIT) do
        unless @fullscreen_toggled
          @window.hide
          @window.resize(@prev_width, @prev_height)
          @window.decorated = true
          @window.show_all
          @window.move(@prev_x, @prev_y)
        end
        false
      end
    end
  end

  class NullFrame
    class << self
      def def_null_methods(*names)
        names.each do |name|
          define_method(name) {}
        end
      end
    end

    def_null_methods(:icon, :icon=, :set_icon)
    def_null_methods(:icon_list, :icon_list=, :set_icon_list)

    def_null_methods(:fullscreen?)
  end
  
end
