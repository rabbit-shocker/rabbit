require 'rabbit/gtk'

module Rabbit
  class Progress
    attr_reader :window, :foreground, :background
    def initialize
      @window = Gtk::Window.new(:popup)
      @window.app_paintable = true
      @bar = Gtk::ProgressBar.new
      @bar.show_text = true
      @window.add(@bar)
      @foreground = nil
      @background = nil
    end

    def foreground=(color)
      @foreground = color
      setup_progress_color
    end

    def background=(color)
      @background = color
      setup_progress_color
    end

    def clear_color
      @foreground = nil
      @background = nil
      setup_progress_color
    end

    def start_progress(max, parent)
      return if max.zero?
      @window.transient_for = parent
      @window.show_all
      @bar.fraction = @current = 0
      @max = max.to_f
    end

    def update_progress(i)
      return if @max.nil?
      @current = i
      @bar.fraction = @current / @max
    end

    def end_progress
      return if @max.nil?
      @current = @max
      @bar.fraction = @current / @max
    end

    private
    def setup_progress_color
      style_context = @bar.style_context
      style_context.save
      if @foreground
        # rgb = @foreground.to_gdk_rgb
        # style_context.set_background(Gtk::StateType::PRELIGHT, *rgb)
      end
      if @background
        # rgb = @background.to_gdk_rgb
        # style_context.set_background(Gtk::StateType::NORMAL, *rgb)
      end
      style_context.restore
    end
  end
end

