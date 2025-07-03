# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require_relative "gtk"

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
      @max = nil
      @window.destroy
      @bar = nil
      @window = nil
    end

    private
    def setup_progress_color
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
    end
  end
end

