require 'erb'

require 'rabbit/dependency-canvas'
require 'rabbit/renderer/display/drawing-area-view-only'
require 'rabbit/renderer/display/hook-handler'
require 'rabbit/renderer/display/key-handler'
require 'rabbit/renderer/display/button-handler'
require 'rabbit/renderer/display/scroll-handler'
require 'rabbit/renderer/display/menu'

module Rabbit
  class InfoWindow
    include ERB::Util

    include Renderer::Display::HookHandler
    include Renderer::Display::KeyHandler
    include Renderer::Display::ButtonHandler
    include Renderer::Display::ScrollHandler
    include Renderer::Display::Menu

    def initialize(canvas)
      @canvas = canvas
      @window = nil
      @timer_started = false
      init_hook_handler
      init_key_handler
      init_button_handler
      init_scroll_handler
    end

    def show(width=nil, height=nil)
      init_gui(width, height)
      @window.show_all
      update_source
      toggle_index_mode if @canvas.index_mode?
      adjust_slide
    end

    def hide
      return unless showing?
      detach_menu(@window)
      detach_key(@window)
      each do |canvas|
        canvas.detach
      end
      @window.signal_handler_disconnect(@window_destroy_id)
      @window.destroy
      @window = @window_destroy_id = nil
      @canvas_widgets = @outer_box = nil
      @timer_started = false
      @previous_canvas = @current_canvas = @next_canvas = nil
    end

    def showing?
      !@window.nil?
    end

    def moved(index)
      return unless showing?
      check_timer
      adjust_slide(index)
    end

    def index_mode_on
      return unless showing?
      toggle_index_mode
    end

    def index_mode_off
      return unless showing?
      toggle_index_mode
    end

    private
    def init_gui(width, height)
      init_canvas
      init_window(width, height)
    end

    def init_canvas
      @current_canvas = make_canvas
      @previous_canvas = make_canvas
      @next_canvas = make_canvas
    end

    def make_canvas
      DependencyCanvas.new(@canvas, @canvas.logger,
                           Renderer::Display::DrawingAreaViewOnly)
    end

    def init_window(width, height)
      @window = Gtk::Window.new
      @window_destroy_id = @window.signal_connect("destroy") do
        @canvas.activate("ToggleInfoWindow")
      end
      @window.title = _("%s: Information window") % @canvas.title
      @window.set_default_size(width, height) if width and height
      init_widgets(width, height)
      init_menu
      attach_key(@window)
      attach_menu(@window)
      event_mask = Gdk::Event::BUTTON_PRESS_MASK
      event_mask |= Gdk::Event::BUTTON_RELEASE_MASK
      event_mask |= Gdk::Event::BUTTON1_MOTION_MASK
      event_mask |= Gdk::Event::BUTTON2_MOTION_MASK
      event_mask |= Gdk::Event::BUTTON3_MOTION_MASK
      @window.add_events(event_mask)
      set_button_event(@window)
      set_scroll_event(@window)
      @window.add(@outer_box)
    end

    def init_widgets(width, height)
      init_timer_label(width * (1.0 / 3.0), height * (1.0 / 3.0))
      @outer_box = Gtk::VBox.new

      current_box = Gtk::HBox.new
      @current_canvas.attach_to(nil, @window, current_box) do |container, widget|
        widget.set_size_request(width * (2.0 / 3.0), height * (2.0 / 3.0))
        container.pack_start(widget, true, false)
      end
      @outer_box.pack_start(current_box, true, false)

      bottom_box = Gtk::HBox.new
      @previous_canvas.attach_to(nil, @window, bottom_box) do |container, widget|
        widget.set_size_request(width * (1.0 / 3.0), height * (1.0 / 3.0))
        container.pack_start(widget, false, false)
      end
      bottom_box.pack_start(@timer_label, true, false)
      @next_canvas.attach_to(nil, @window, bottom_box) do |container, widget|
        widget.set_size_request(width * (1.0 / 3.0), height * (1.0 / 3.0))
        container.pack_end(widget, false, false)
      end
      @outer_box.pack_end(bottom_box, false, false)

      @outer_box.show
    end

    def init_canvas_widgets
      @canvas_widgets = Gtk::HBox.new
      @current_canvas.attach_to(nil, @window, @canvas_widgets)
      @next_canvas.attach_to(nil, @window, @canvas_widgets)
    end

    def init_timer_label(width, height)
      @timer_label = Gtk::Label.new
      @timer_label.justify = :center
      @timer_label.markup = markupped_timer_label(width, height)
      @timer_started = false
      check_timer
    end

    def check_timer
      return if @timer_started

      Gtk.timeout_add(1000) do
        @timer_label.markup = markupped_timer_label if showing?
        @timer_started = (showing? and @canvas.rest_time)
      end
    end

    def markupped_timer_label(width=nil, height=nil)
      width ||= @window.size[0] * (1.0 / 3.0)
      height ||= @window.size[1] * (1.0 / 3.0)
      attrs = {}
      attrs["font_desc"] = ((height * 200) / Pango::SCALE).to_s
      rest_time = @canvas.rest_time
      attrs["foreground"] = "red" if rest_time and rest_time < 0
      "<span #{@canvas.to_attrs(attrs)}>#{h timer_label}</span>"
    end

    def timer_label
      rest_time = @canvas.rest_time || @canvas.allotted_time
      if rest_time
        "%s%02d:%02d" % Utils.split_number_to_minute_and_second(rest_time)
      else
        _("unlimited")
      end
    end

    def update_source
      each do |canvas|
        source = Source::Memory.new("UTF-8", @canvas.logger)
        @canvas.source_force_modified(true) do |original_source|
          source.source = original_source.read
          source.base = original_source.base
        end
        canvas.parse(source)
      end
    end

    def adjust_slide(base_index=nil)
      base_index ||= @canvas.current_index
      @previous_canvas.move_to_if_can([base_index - 1, 0].max)
      @current_canvas.move_to_if_can(base_index)
      @next_canvas.move_to_if_can([base_index + 1, @canvas.slide_size - 1].min)
    end

    def toggle_index_mode
      each do |canvas|
        canvas.toggle_index_mode
      end
    end

    def each(&block)
      [@previous_canvas, @current_canvas, @next_canvas].each(&block)
    end
  end
end
