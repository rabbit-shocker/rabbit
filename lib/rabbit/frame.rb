require "forwardable"
require "gtk3"
require "rexml/text"

require "rabbit/rabbit"
require "rabbit/utils"

module Rabbit

  class Frame

    include ScreenInfo
    extend Forwardable

    def_delegators(:@window, :icon, :icon=, :set_icon)
    def_delegators(:@window, :icon_list, :icon_list=, :set_icon_list)
    def_delegators(:@window, :iconify, :show, :hide, :visible?)
    def_delegators(:@window, :set_size_request, :resize)

    def_delegators(:@canvas, :apply_theme, :theme_name)
    def_delegators(:@canvas, :saved_image_type=, :saved_image_base_name=)
    def_delegators(:@canvas, :save_as_image, :each_slide_pixbuf)
    def_delegators(:@canvas, :print, :filename=)
    def_delegators(:@canvas, :output_html=, :output_index_html=)
    def_delegators(:@canvas, :rss_base_uri=, :use_gl?, :use_gl=)

    attr_reader :window, :logger
    attr_accessor :geometry, :force_keep_above

    def initialize(logger, canvas)
      @logger = logger
      @canvas = canvas
      @geometry = nil
      @force_keep_above = nil
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

    def parse(source, callback=nil, &block)
      @canvas.parse(source, callback, &block)
    end

    def fullscreen
      @fullscreen_toggled = false
      @fullscreen = true
      @window.fullscreen
    end

    def unfullscreen
      @fullscreen_toggled = false
      @fullscreen = false
      @window.unfullscreen
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
      if @main_window
        @window.keep_above = @force_keep_above unless @force_keep_above.nil?
      else
        @window.keep_above = true
      end
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
      window_type ||= Gtk::Window::Type::TOPLEVEL
      @window = Gtk::Window.new(window_type)
      @window.set_default_size(width, height)
      @window.parse_geometry(@geometry) if @geometry
      @window.set_app_paintable(true)
      set_window_signal
      setup_dnd
      @canvas.attach_to(self, @window)
    end

    def set_window_signal
      set_window_signal_window_state_event
      set_window_signal_destroy
    end

    def update_keep_above(keep_above=nil)
      if @main_window
        keep_above = @force_keep_above unless @force_keep_above.nil?
        @window.keep_above = keep_above unless keep_above.nil?
      else
        @window.keep_above = true
      end
    end

    def set_window_signal_window_state_event
      @window.signal_connect("window_state_event") do |widget, event|
        if event.changed_mask.fullscreen?
          @fullscreen_toggled = true
          if fullscreen?
            @window.keep_above = true
            @canvas.fullscreened
          else
            update_keep_above(false)
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
        @canvas.detach
        if main_window? and Gtk.main_level > 0
          Gtk.main_quit
        end
      end
    end

    def setup_dnd
      Gtk::Drag.dest_set(@window,
                         Gtk::Drag::DEST_DEFAULT_ALL,
                         [["text/uri-list", 0, 0],
                          ["_NETSCAPE_URL", 0, 0]],
                         Gdk::DragContext::ACTION_COPY)
      @window.signal_connect("drag-data-received") do |*args|
        widget, context, x, y, selection_data, info, time = args
        uri = selection_data.data.chomp
        Gtk.idle_add do
          parse(Source::URI.new(nil, logger, uri))
          false
        end
        Gtk::Drag.finish(context, true, false, time)
      end

      @window.signal_connect("drag-drop") do |widget, context, x, y, time|
        true
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
    def_null_methods(:update_title, :geometry, :geometry=)
    def_null_methods(:force_keep_above, :force_keep_above=)

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
