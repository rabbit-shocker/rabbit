require 'gtk2'

module Rabbit
  class Progress
    attr_reader :window, :foreground, :background
    def initialize
      @window = Gtk::Window.new(Gtk::Window::POPUP)
      @window.app_paintable = true
      @bar = Gtk::ProgressBar.new
      @bar.show_text = true
      @window.add(@bar)
      @original_style = @bar.style
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
      @current = i
      @bar.fraction = @current / @max
    end

    def end_progress
      @current = @max
      @bar.fraction = @current / @max
    end

    private
    def setup_progress_color
      style = @original_style.copy
      if @foreground
        rgb = @foreground.to_gdk_rgb
        style.set_bg(Gtk::STATE_NORMAL, *rgb)
      end
      if @background
        rgb = @background.to_gdk_rgb
        style.set_bg(Gtk::STATE_PRELIGHT, *rgb)
      end
      @bar.style = style
    end
  end
end

