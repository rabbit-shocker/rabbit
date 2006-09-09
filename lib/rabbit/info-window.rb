require 'erb'

require 'rabbit/dependency-canvas'
require 'rabbit/renderer/display/drawing-area-view-only'

module Rabbit
  class InfoWindow
    include ERB::Util

    def initialize(canvas)
      @canvas = canvas
      @window = nil
      @timer_started = false
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
      each do |canvas|
        canvas.detach
      end
      @window.signal_handler_disconnect(@window_destroy_id)
      @window.destroy
      @window = @window_destroy_id = nil
      @hbox = @vbox = nil
      @timer_started = false
      @previous_canvas = @next_canvas = nil
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
      init_vbox
      @window.add(@vbox)
    end

    def init_vbox
      @vbox = Gtk::VBox.new
      init_hbox
      init_timer_label
      @vbox.pack_start(@hbox, true, true)
      @vbox.pack_end(@timer_label, true, true)
      @vbox.show
    end

    def init_hbox
      @hbox = Gtk::HBox.new
      @previous_canvas.attach_to(nil, @hbox)
      @next_canvas.attach_to(nil, @hbox)
      @hbox.show
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
      rest_time = @canvas.rest_time
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
        canvas.parse_rd(source)
      end
    end

    def adjust_slide(base_index=nil)
      base_index ||= @canvas.current_index
      @previous_canvas.move_to_if_can(base_index - 1)
      @next_canvas.move_to_if_can(base_index + 1)
    end

    def toggle_index_mode
      each do |canvas|
        canvas.toggle_index_mode
      end
    end

    def each(&block)
      [@previous_canvas, @next_canvas].each(&block)
    end
  end
end
