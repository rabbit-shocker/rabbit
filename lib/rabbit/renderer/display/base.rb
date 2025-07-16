# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "hook-handler"

module Rabbit
  module Renderer
    module Display
      module Base
        include HookHandler

        def initialize(*args, &block)
          @surface = nil
          @size = nil
          @size_dirty = true
          super
        end

        def width
          refresh_size
          if @size
            @size.logical_width
          else
            nil
          end
        end

        def height
          refresh_size
          if @size
            @size.logical_height
          else
            nil
          end
        end

        def size
          refresh_size
          @size
        end

        def update_size(w, h)
          return if @real_width == w and @real_height == h

          @real_width = w
          @real_height = h
          @size_dirty = true
        end

        def redraw
          widget.queue_draw
        end

        def attach_to(window, container=nil)
          @window = window
          @container = container || @window
        end

        def detach
          @window = nil
          @container = nil
        end

        def toggle_whiteout
          super
          update_menu
        end

        def toggle_blackout
          super
          update_menu
        end

        def make_layout(text)
          attrs, text = Pango.parse_markup(text)
          layout = create_pango_layout(text)
          layout.set_attributes(attrs)
          layout
        end

        def create_pango_context
          context = widget.create_pango_context
          set_font_resolution(context)
          context
        end

        def create_pango_layout(text)
          layout = widget.create_pango_layout(text)
          set_font_resolution(layout.context)
          layout
        end

        def update_title
          @canvas.update_title(@canvas.slide_title)
        end

        def draw_slide(slide, simulation)
          set_size_ratio(slide.size_ratio)

          if simulation
            super
          else
            save_context do
              scale_context(*@size.logical_scale)
              translate_context(@size.logical_margin_left,
                                @size.logical_margin_top)
              super
            end

            unless @size.have_logical_margin?
              return
            end

            margin_background = make_color("black")
            save_context do
              scale_context(*@size.logical_scale)
              if @size.have_logical_margin_x?
                draw_rectangle(true,
                               0,
                               0,
                               @size.logical_margin_left,
                               @size.logical_height,
                               margin_background)
                draw_rectangle(true,
                               @size.logical_margin_left + @size.logical_width,
                               0,
                               @size.logical_margin_right,
                               @size.logical_height,
                               margin_background)
              end
              if @size.have_logical_margin_y?
                draw_rectangle(true,
                               0,
                               0,
                               @size.logical_width,
                               @size.logical_margin_top,
                               margin_background)
                draw_rectangle(true,
                               0,
                               @size.logical_margin_top + @size.logical_height,
                               @size.logical_width,
                               @size.logical_margin_bottom,
                               margin_background)
              end
            end
          end
        end

        private
        def set_surface(surface)
          @surface = surface
          set_default_size(@surface.width, @surface.height)
        end

        def set_default_size(w, h)
          @real_width = w
          @real_height = h
          ratio = @base_width.to_f / @base_height.to_f
          set_size(w, h, ratio)
        end

        def set_size(w, h, ratio)
          @size = Size.new(@base_width,
                           @base_height,
                           w,
                           h,
                           ratio)
        end

        def set_size_ratio(ratio)
          ratio ||= @base_width.to_f / @base_height.to_f
          return if @size.ratio == ratio

          w = @size.real_width
          h = @size.real_height
          set_size(w, h, ratio)
        end

        def refresh_size
          return unless @size_dirty
          set_size(@real_width, @real_height, @size.ratio)
          @size_dirty = false
        end

        def queue_draw
          widget.queue_draw
        end
      end
    end
  end
end
