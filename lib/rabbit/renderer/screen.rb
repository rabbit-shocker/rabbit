# Copyright (C) 2018-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "base"

require_relative "display/base"
require_relative "display/menu"
require_relative "display/progress"
require_relative "display/mask"
require_relative "display/search"
require_relative "display/gesture"
require_relative "display/graffiti"
require_relative "display/button-handler"
require_relative "display/scroll-handler"
require_relative "display/info"
require_relative "display/spotlight"
require_relative "display/magnifier"

require_relative "widget/drawing-area"

module Rabbit
  module Renderer
    class Screen
      extend Forwardable

      include Display::Base

      include Display::Cursor
      # include Display::Menu
      # include Display::Graffiti
      # include Display::Mask
      # include Display::Progress
      # include Display::Search
      # include Display::Gesture
      # include Display::ButtonHandler
      # include Display::ScrollHandler
      # include Display::Info
      # include Display::Spotlight
      # include Display::Magnifier

      def_delegators(:@slide_widget, :make_color)
      def_delegators(:@slide_widget, :foreground=)
      def_delegators(:@slide_widget, :background=)
      def_delegators(:@slide_widget, :set_font_resolution)

      def_delegators(:@slide_widget, :flag_size)

      def_delegators(:@slide_widget, :draw_layout)
      def_delegators(:@slide_widget, :draw_pixbuf)
      def_delegators(:@slide_widget, :draw_line)
      def_delegators(:@slide_widget, :draw_rectangle)
      def_delegators(:@slide_widget, :draw_rsvg_handle)
      def_delegators(:@slide_widget, :draw_link)
      def_delegators(:@slide_widget, :draw_flag)

      attr_accessor :filename
      def initialize(canvas)
        super
        @filename = nil
        init_ui
      end

      def attach_to(window, container=nil)
        super
        if container
          container.add(@fixed)
        else
          @window.child = @fixed
        end
        @fixed.show
        @key_handler = KeyHandler.new(@canvas, @window)
      end

      def detach
        @key_handler.detach
        @key_handler = nil
        @fixed.hide
        super
      end

      def widget
        @fixed
      end

      def queue_draw
        @slide_widget.queue_draw
      end

      def clear_slide
        super
        @slide_widget.clear_compiled_slide
        redraw
      end

      def post_fullscreen
        update_cursor(:blank, true)
        @slide_widget.clear_compiled_slides
      end

      def post_unfullscreen
        update_cursor(nil, true)
        update_menu
      end

      def post_iconify
      end

      def post_apply_theme
        @slide_widget.clear_compiled_slides
        queue_draw
      end

      def post_move(old_index, index)
        queue_draw
      end

      def post_move_in_slide(old_index, index)
        queue_draw
      end

      def pre_parse
      end

      def post_parse
        @slide_widget.clear_compiled_slides
      end

      def pre_toggle_index_mode
      end

      def post_toggle_index_mode
        queue_draw
      end

      def pre_to_pixbuf(slide_size)
      end

      def to_pixbufing(i)
        true
      end

      def post_to_pixbuf(canceled)
      end

      def index_mode_on
        keep_cursor(:index)
        update_cursor(nil, true)
      end

      def index_mode_off
        restore_cursor(:index)
      end

      # TODO: screen?
      def display?
        true
      end

      def draw_slide(slide, simulation, &block)
        set_size_ratio(slide.size_ratio || @default_size_ratio)

        if simulation
          @slide_widget.draw_slide(slide, simulation, &block)
        else
          @slide_widget.save_context do
            @slide_widget.scale_context(*@size.logical_scale)
            @slide_widget.translate_context(@size.logical_margin_left,
                                            @size.logical_margin_top)
            @slide_widget.draw_slide(slide, simulation, &block)
          end

          unless @size.have_logical_margin?
            return
          end

          margin_background = @slide_widget.make_color("black")
          if @size.have_logical_margin_x?
            @slide_widget.draw_rectangle(true,
                                         0,
                                         0,
                                         @size.logical_margin_left,
                                         @size.real_height,
                                         margin_background)
            @slide_widget.draw_rectangle(true,
                                         @size.real_width - @size.logical_margin_right,
                                         0,
                                         @size.logical_margin_right,
                                         @size.real_height,
                                         margin_background)
          end
          if @size.have_logical_margin_y?
            @slide_widget.draw_rectangle(true,
                                         0,
                                         0,
                                         @size.real_width,
                                         @size.logical_margin_top,
                                         margin_background)
            @slide_widget.draw_rectangle(true,
                                         0,
                                         @size.real_height - @size.logical_margin_bottom,
                                         @size.real_width,
                                         @size.logical_margin_bottom,
                                         margin_background)
          end
        end
      end

      private
      def init_ui
        @fixed = Gtk::Fixed.new
        @fixed.can_focus = true
        set_map
        set_size_allocate

        set_key_press_event(@fixed)

        @slide_widget = Widget::DrawingArea.new(@canvas)
        @slide_widget.raw.show
        @fixed.put(@slide_widget.raw, 0, 0)
      end

      def depth
        @fixed.window.depth
      end

      def set_map
        @fixed.signal_connect_after(:map) do |widget|
          mapped(widget)
        end
      end

      def mapped(widget)
        allocation = widget.allocation
        set_default_size(allocation.width,
                         allocation.height)
      end

      def set_size_allocate
        @fixed.signal_connect(:size_allocate) do |widget, allocation|
          w = allocation.width
          h = allocation.height
          @slide_widget.raw.allocation = allocation
          update_size(w, h)
          reload_theme
        end
      end

      def reload_theme(&callback)
        callback ||= Utils.process_pending_events_proc
        @canvas.activate("ReloadTheme", &callback)
      end

      def grab
        @fixed.grab_add
        Gdk.pointer_grab(@fixed.window, false,
                         Gdk::EventMask::BUTTON_PRESS_MASK |
                         Gdk::EventMask::BUTTON_RELEASE_MASK |
                         Gdk::EventMask::SCROLL_MASK |
                         Gdk::EventMask::POINTER_MOTION_MASK,
                         nil, nil,
                         Gdk::CURRENT_TIME)
      end

      def ungrab
        @fixed.grab_remove
        Gdk.pointer_ungrab(Gdk::CURRENT_TIME)
      end

      def pointer
        window, x, y, mask = @fixed.window.pointer
        [x, y, mask]
      end
    end
  end
end
