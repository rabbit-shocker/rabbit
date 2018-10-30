# Copyright (C) 2006-2018  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/renderer/engine/cairo"

module Rabbit
  module Renderer
    module Widget
      class DrawingArea
        include Engine::Cairo

        def initialize(canvas)
          @canvas = canvas
          @area = Gtk::DrawingArea.new
          clear_compiled_slides
          set_draw
        end

        def raw
          @area
        end

        def queue_redraw
          @area.queue_redraw
        end

        def clear_compiled_slide(slide=nil)
          @compiled_slides.delete(slide || @canvas.current_slide)
        end

        def clear_compiled_slides
          @compiled_slides = {}
        end

        def width
          @area.allocation.width
        end

        def height
          @area.allocation.height
        end

        private
        def set_draw
          @area.signal_connect(:draw) do |widget, context|
            init_context(context)
            draw(widget)
            finish_renderer
            Gdk::Event::PROPAGATE
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
              @canvas.logger.warn($!)
            end
          end
        end

        def compiled_slide?(slide)
          @compiled_slides.has_key?(slide)
        end

        def compile_slide(slide)
          @compiled_slides[slide] = true
          slide.draw(@canvas, true)
        end
      end
    end
  end
end
