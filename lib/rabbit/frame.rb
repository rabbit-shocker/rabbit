require "forwardable"
require "rabbit/gtk"
require "rexml/text"

begin
  case ENV["RABBIT_GTK"]
  when "4"
    require "vte4"
  else
    require "vte3"
  end
rescue LoadError
end

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
    attr_accessor :geometry

    def initialize(logger, canvas)
      @logger = logger
      @canvas = canvas
      @geometry = nil
      @notebook = nil
      @terminal = nil
      @running = true
    end

    def destroyed?
      @window.nil? or @window.destroyed?
    end

    def quit
      @running = false
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
      @window.fullscreen
    end

    def unfullscreen
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
      @fullscreen
    end

    def main_window?
      @main_window
    end

    def update_title(new_title)
      @window.title = Utils.unescape_title(new_title)
    end

    def init_gui(width, height, main_window, window_type=nil)
      init_window(width, height, window_type)
      @fullscreen = false
      @main_window = main_window
      @terminal.show if @terminal
      @notebook.show if @notebook
      @window.show
      @canvas.post_init_gui
    end

    def fullscreen_available?
      true
    end

    def iconify_available?
      true
    end

    def toggle_terminal
      return if @terminal.nil?
      terminal_page = @notebook.page_num(@terminal)
      if @notebook.current_page == terminal_page
        @notebook.current_page = 0
      else
        @notebook.current_page = terminal_page
      end
    end

    def in_terminal?
      return false if @terminal.nil?
      @notebook.current_page == @notebook.page_num(@terminal)
    end

    private
    def init_window(width, height, window_type=nil)
      window_type ||= :toplevel
      @window = Gtk::ApplicationWindow.new(::Rabbit.application)
      @window.set_default_size(width, height)
      @window.parse_geometry(@geometry) if @geometry
      @window.set_app_paintable(true)
      if defined?(Vte::Terminal)
        init_notebook
      end
      set_window_signal
      setup_dnd
      @canvas.attach_to(self, @window, @notebook)
      if defined?(Vte::Terminal)
        init_terminal
      end
    end

    def init_notebook
      @notebook = Gtk::Notebook.new
      @notebook.show_tabs = false
      provider = Gtk::CssProvider.new
      provider.load(data: <<-CSS)
        notebook {
          border-width: 0px;
        }
      CSS
      @notebook.style_context.add_provider(provider,
                                           Gtk::StyleProvider::PRIORITY_USER)
      @window.add(@notebook)
    end

    def set_window_signal
      set_window_signal_window_state_event
      set_window_signal_destroy
    end

    def set_window_signal_window_state_event
      @window.signal_connect("window_state_event") do |widget, event|
        if event.changed_mask.fullscreen?
          @fullscreen = event.new_window_state.fullscreen?
          if @fullscreen
            @canvas.fullscreened
          else
            @canvas.unfullscreened
          end
        elsif event.changed_mask.iconified?
          if event.new_window_state.iconified?
            @canvas.iconified
          end
        end

        false
      end
    end

    def set_window_signal_destroy
      @window.signal_connect("destroy") do
        @canvas.detach
      end
    end

    def setup_dnd
      @window.drag_dest_set(:all,
                            [["text/uri-list", 0, 0],
                             ["_NETSCAPE_URL", 0, 0]],
                            :copy)
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

    def init_terminal
      @terminal = Vte::Terminal.new
      # TODO: Support theme
      terminal_font_description = ENV["RABBIT_TERMINAL_FONT_DESCRIPTION"]
      if terminal_font_description
        @terminal.font_desc =
          Pango::FontDescription.new(terminal_font_description)
      end
      terminal_color_foreground = ENV["RABBIT_TERMINAL_COLOR_FOREGROUND"]
      if terminal_color_foreground
        @terminal.color_foreground = terminal_color_foreground
      end
      terminal_color_background = ENV["RABBIT_TERMINAL_COLOR_BACKGROUND"]
      if terminal_color_background
        @terminal.color_background = terminal_color_background
      end
      @terminal.enable_sixel = true if @terminal.respond_to?(:enable_sixel=)
      @notebook.add(@terminal)
      pid = nil
      in_terminal = false
      @notebook.signal_connect(:switch_page) do |_, page,|
        if page == @terminal
          if @running
            pid = @terminal.spawn if pid.nil?
            @canvas.pre_terminal unless in_terminal
            in_terminal = true
          end
        else
          @canvas.post_terminal if in_terminal
          in_terminal = false
        end
      end
      @terminal.signal_connect(:child_exited) do
        pid = nil
        terminal_page = @notebook.page_num(@terminal)
        if @notebook.current_page == terminal_page
          @canvas.activate("ToggleTerminal")
        end
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

    def_null_methods(:fullscreen?, :quit)

    def fullscreen_available?
      false
    end

    def iconify_available?
      false
    end

    def toggle_terminal
    end

    def in_terminal?
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

    def toggle_terminal
    end

    def in_terminal?
      false
    end

    def init_gui(width, height, main_window, window_type=nil)
      @window = Gtk::EventBox.new
      @window.set_size_request(width, height)
      @canvas.attach_to(self, @window)
      @fullscreen = false
      @main_window = main_window
      @window.show
      @canvas.post_init_gui
    end
  end
end
