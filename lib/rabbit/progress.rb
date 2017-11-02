require 'rabbit/gtk'

module Rabbit
  class Progress
    attr_reader :window, :foreground, :background
    def initialize
      @width = 100
      @height = 20
      @foreground = nil
      @background = nil
    end

    def foreground=(color)
      @foreground = color
    end

    def background=(color)
      @background = color
    end

    def clear_color
      @foreground = nil
      @background = nil
    end

    def start_progress(max, parent)
      return if max.zero?

      @window = Gtk::Window.new(:popup)
      @window.transient_for = parent
      @window.app_paintable = true
      @window.set_default_size(@width, @height)
      @bar = Gtk::ProgressBar.new
      @window.add(@bar)
      @window.show_all
      @bar.fraction = @current = 0
      @max = max.to_f

      setup_progress_color
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

    def hide
      @window.destroy
      @bar = nil
      @window = nil
    end

    private
    def setup_progress_color
      if Gtk.const_defined?(:CssProvider)
        css = <<-CSS
progress {
  padding: 0px;
  padding-top: #{@height}px;
  padding-bottom: #{@height}px;
}

trough {
  border-width: 0px;
}
        CSS
        if @foreground
          css << <<-CSS
progress {
  background-color: #{@foreground.to_css_rgba};
  border-color: #{@foreground.to_css_rgba};
}
          CSS
        end
        if @background
          css << <<-CSS
progressbar,
trough {
  background-color: #{@background.to_css_rgba};
}
          CSS
        end
        provider = Gtk::CssProvider.new
        provider.load(:data => css)
        @bar.style_context.add_provider(provider,
                                        Gtk::StyleProvider::PRIORITY_USER)
      else
        style = @bar.style.copy
        if @foreground
          rgb = @foreground.to_gdk_rgb
          style.set_bg(Gtk::STATE_PRELIGHT, *rgb)
        end
        if @background
          rgb = @background.to_gdk_rgb
          style.set_bg(Gtk::STATE_NORMAL, *rgb)
        end
        @bar.style = style
      end
    end
  end
end

