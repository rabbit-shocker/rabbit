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

require "forwardable"
require "rexml/text"

require_relative "gtk"

begin
  case ENV["RABBIT_GTK"]
  when "4"
    require "vte4"
  else
    require "vte3"
  end
rescue LoadError
end

require_relative "rabbit"
require_relative "utils"

module Rabbit
  class Frame
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

    attr_reader :window
    attr_accessor :geometry

    def initialize(canvas)
      @canvas = canvas
      @geometry = nil
      @stack = nil
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
      @stack.show if @stack
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
      if @stack.visible_child_name == "terminal"
        if @stack.respond_to?(:pages)
          @stack.visible_child = @stack.pages[0].child
        else
          @stack.visible_child = @stack.children[0]
        end
      else
        @stack.visible_child_name = "terminal"
      end
    end

    def in_terminal?
      return false if @terminal.nil?
      @stack.visible_child_name == "terminal"
    end

    private
    def init_window(width, height, window_type=nil)
      window_type ||= :toplevel
      @window = Gtk::ApplicationWindow.new(::Rabbit.application)
      @window.set_default_size(width, height)
      @window.parse_geometry(@geometry) if @geometry
      if defined?(Vte::Terminal)
        init_stack
      end
      set_window_signal_destroy
      setup_dnd
      @window.show # @window.surface is only available after @window.show.
      if GTK::Version::MAJOR < 4
        @window.signal_connect(:configure_event) do |_, event|
          @canvas.renderer.update_size(event.width, event.height)
          false
        end
        @window.signal_connect(:window_state_event) do |widget, event|
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
      else
        previous_width = nil
        previous_height = nil
        previous_fullscreen = @fullscreen = @window.fullscreen?
        previous_minimized = false
        @surface = @window.surface # This is to guard from GC.
        @surface.signal_connect(:notify) do |surface, param|
          case param.name
          when "width", "height"
            width = surface.width
            height = surface.height
            if previous_width != width or previous_height != height
              @canvas.renderer.update_size(width, height)
              previous_width = width
              previous_height = height
            end
          when "state"
            state = surface.state
            fullscreen = state.fullscreen?
            if previous_fullscreen != fullscreen
              @fullscreen = fullscreen
              if @fullscreen
                @canvas.fullscreened
              else
                @canvas.unfullscreened
              end
              previous_fullscreen = @fullscreen
            end
            minimized = state.minimized?
            if previous_minimized != minimized
              @canvas.iconified if minimized
              previous_minimized = minimized
            end
          end
        end
      end
      @canvas.attach_to(self, @window, @stack)
      if defined?(Vte::Terminal)
        init_terminal
      end
    end

    def init_stack
      @stack = Gtk::Stack.new
      if Gtk::StackTransitionType.const_defined?(:ROTATE_LEFT_RIGHT)
        @stack.transition_type = :rotate_left_right
      else
        @stack.transition_type = :slide_left_right
      end
      @window.child = @stack
    end

    def set_window_signal_destroy
      @window.signal_connect("destroy") do
        @canvas.detach
      end
    end

    def setup_dnd
      return if GTK::Version::MAJOR >= 4 # TODO
      @window.drag_dest_set(:all,
                            [["text/uri-list", 0, 0],
                             ["_NETSCAPE_URL", 0, 0]],
                            :copy)
      @window.signal_connect("drag-data-received") do |*args|
        widget, context, x, y, selection_data, info, time = args
        uri = selection_data.data.chomp
        Gtk.idle_add do
          parse(Source::URI.new(nil, uri))
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
      @stack.add_named(@terminal, "terminal")
      pid = nil
      in_terminal = false
      @stack.signal_connect("notify::visible-child-name") do |_, param|
        if @stack.visible_child_name == "terminal"
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
        if @stack.visible_child_name == "terminal"
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

    def_null_methods(:quit)

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

    def fullscreen?
      false
    end
  end
end
