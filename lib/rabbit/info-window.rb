require 'erb'

require 'rabbit/dependency-canvas'
require 'rabbit/renderer/display/drawing-area-view-only'
require 'rabbit/renderer/display/key-handler'
require 'rabbit/renderer/display/button-handler'

module Rabbit
  class InfoWindow
    include ERB::Util

    include Renderer::Display::HookHandler
    include Renderer::Display::KeyHandler
    include Renderer::Display::ButtonHandler

    def initialize(canvas)
      @canvas = canvas
      @window = nil
      @timer_started = false
      init_hook_handler
      init_key_handler
      init_button_handler
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
      init_widgets
      attach_key(@window)
      event_mask = Gdk::Event::BUTTON_PRESS_MASK
      event_mask |= Gdk::Event::BUTTON_RELEASE_MASK
      event_mask |= Gdk::Event::BUTTON1_MOTION_MASK
      event_mask |= Gdk::Event::BUTTON2_MOTION_MASK
      event_mask |= Gdk::Event::BUTTON3_MOTION_MASK
      @window.add_events(event_mask)
      set_button_event(@window)
      @window.add(@outer_box)
    end

    def init_widgets
      @outer_box = Gtk::VBox.new
      init_canvas_widgets
      init_timer_label
      @outer_box.pack_start(@canvas_widgets, true, true)
      @outer_box.pack_end(@timer_label, true, true)
      @outer_box.show
    end

    def init_canvas_widgets
      @canvas_widgets = Gtk::HBox.new
      @previous_canvas.attach_to(nil, @window, @canvas_widgets)
      @current_canvas.attach_to(nil, @window, @canvas_widgets)
      @next_canvas.attach_to(nil, @window, @canvas_widgets)
      @canvas_widgets.show
    end

    def init_timer_label
      @timer_label = Gtk::Label.new
      @timer_label.markup = markupped_timer_label
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

    def markupped_timer_label
      attrs = {}
      attrs["font_desc"] = ((@window.size[1] * 100) / Pango::SCALE).to_s
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
      @previous_canvas.move_to_if_can(base_index - 1)
      @current_canvas.move_to_if_can(base_index)
      @next_canvas.move_to_if_can(base_index + 1)
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
