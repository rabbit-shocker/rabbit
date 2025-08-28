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

require_relative "../../gtk"

require_relative "../../utils"
require_relative "../engine"
require_relative "base"
require_relative "cursor"

module Rabbit
  module Renderer
    module Display
      module DrawingAreaPrimitive
        include Base

        include Cursor

        attr_accessor :filename
        def initialize(canvas)
          super
          @filename = nil
          clear_compiled_slides
          init_drawing_area
        end

        def widget
          @area
        end

        def clear_slide
          super
          clear_compiled_slide
          redraw
        end

        def post_fullscreen
          update_cursor(:none, true)
          clear_compiled_slides
        end

        def post_unfullscreen
          update_cursor(nil, true)
          update_menu
        end

        def post_iconify
        end

        def post_apply_theme
          clear_compiled_slides
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
          clear_compiled_slides
          update_cursor(:none, true)
        end

        def pre_toggle_index_mode
        end

        def post_toggle_index_mode
          queue_draw
        end

        def pre_to_pixbuf(n_slides)
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

        def display?
          true
        end

        private
        def init_drawing_area
          @area = Gtk::DrawingArea.new
          @area.can_focus = true
          set_map
          set_draw
        end

        def depth
          @area.window.depth
        end

        def set_map
          @area.signal_connect_after("map") do |widget|
            mapped(widget)
          end
        end

        def mapped(widget)
          set_surface(widget.native.surface)
        end

        def set_draw
          if @area.respond_to?(:set_draw_func)
            @area.set_draw_func do |area, context|
              surface = area.native.surface
              update_size(surface.width, surface.height)
              init_context(context)
              draw(area)
              finish_renderer
            end
          else
            @area.signal_connect("draw") do |widget, context|
              init_context(context)
              draw(widget)
              finish_renderer
              Gdk::Event::PROPAGATE
            end
          end
        end

        def draw(widget)
          draw_current_slide
        end

        def draw_current_slide
          slide = @canvas.current_slide
          if slide
            begin
              compile_slide(slide) unless compiled_slide?(slide)
              slide.draw(@canvas, false)
            rescue
              Rabbit.logger.warn($!)
            end
          end
        end

        def configured(x, y, w, h)
          @real_width = @surface.width
          @real_height = @surface.height
          @size_dirty = true
        end

        def reload_theme(&callback)
          callback ||= Utils.process_pending_events_proc
          @canvas.activate("ReloadTheme", &callback)
        end

        def clear_compiled_slide(slide=nil)
          @compiled_slides.delete(slide || @canvas.current_slide)
        end

        def clear_compiled_slides
          @compiled_slides = {}
        end

        def compiled_slide?(slide)
          @compiled_slides.has_key?(slide)
        end

        def compile_slide(slide)
          @compiled_slides[slide] = true
          slide.draw(@canvas, true)
        end

        def grab
          return unless @area.respond_to?(:grab_add)
          @area.grab_add
          Gdk.pointer_grab(@area.window, false,
                           Gdk::EventMask::BUTTON_PRESS_MASK |
                           Gdk::EventMask::BUTTON_RELEASE_MASK |
                           Gdk::EventMask::SCROLL_MASK |
                           Gdk::EventMask::POINTER_MOTION_MASK,
                           nil, nil,
                           Gdk::CURRENT_TIME)
        end

        def ungrab
          return unless @area.respond_to?(:grab_remove)
          @area.grab_remove
          Gdk.pointer_ungrab(Gdk::CURRENT_TIME)
        end
      end
    end
  end
end
