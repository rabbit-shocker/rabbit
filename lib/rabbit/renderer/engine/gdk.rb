require 'gtk2'

require 'rabbit/renderer/kernel'

module Rabbit
  module Renderer
    module Engine
      module GDK
        include Kernel

        class << self
          def priority
            0
          end
        end

        def init_renderer(drawable)
          @gdk_drawable = drawable
        end

        def finish_renderer
          @gdk_drawable = nil
        end

        def alpha_available?
          false
        end

        def foreground=(color)
          @foreground.set_rgb_fg_color(color.to_gdk_color)
        end

        def background=(color)
          @background.set_rgb_fg_color(color.to_gdk_color)
        end

        def background_image=(pixbuf)
          w, h = pixbuf.width, pixbuf.height
          pixmap = Gdk::Pixmap.new(nil, w, h, depth)
          pixmap.draw_rectangle(@background, true, 0, 0, w, h)
          args = [
                  @foreground, pixbuf,
                  0, 0, 0, 0, w, h,
                  Gdk::RGB::DITHER_NORMAL, 0, 0,
                 ]
          pixmap.draw_pixbuf(*args)
          @background.set_tile(pixmap)
          @background.fill = Gdk::GC::Fill::TILED
        end

        def draw_line(x1, y1, x2, y2, color=nil, params={})
          gc = make_gc(color, params)
          return if gc.nil?
          @gdk_drawable.draw_line(gc, x1, y1, x2, y2)
        end

        def draw_lines(points, color=nil, params={})
          gc = make_gc(color, params)
          return if gc.nil?
          @gdk_drawable.draw_lines(gc, points)
        end

        def draw_rectangle(filled, x, y, w, h, color=nil, params={})
          gc = make_gc(color, params)
          return if gc.nil?
          @gdk_drawable.draw_rectangle(gc, filled, x, y, w, h)
        end

        def draw_rounded_rectangle(filled, x, y, w, h, radius, color=nil, params={})
          x_radius = params[:x_radius] || radius
          y_radius = params[:y_radius] || radius
          x_diameter = x_radius * 2
          y_diameter = y_radius * 2

          line_width = params[:line_width]

          inner_x = x + x_radius
          inner_y = y + y_radius
          inner_w = w - x_diameter
          inner_h = h - y_diameter

          if filled
            draw_rectangle(true, inner_x, inner_y, inner_w, inner_h, color)
          end

          if filled
            top = [inner_x, y, inner_w, y_radius]
            left = [x, inner_y, x_radius, inner_h]
            bottom = [inner_x, inner_y + inner_h, inner_w, y_radius]
            right = [inner_x + inner_w, inner_y, x_radius, inner_h]

            [top, left, bottom, right].each do |rx, ry, rw, rh|
              draw_rectangle(true, rx, ry, rw, rh, color)
            end
          else
            top = [inner_x, y, inner_x + inner_w, y]
            left = [x, inner_y, x, inner_y + inner_h]
            bottom = [inner_x, y + h, inner_x + inner_w, y + h]
            right = [x + w, inner_y, x + w, inner_y + inner_h]
            [top, left, bottom, right].each do |start_x, start_y, end_x, end_y|
              draw_line(start_x, start_y, end_x, end_y, color,
                        {:line_width => line_width})
            end
          end

          upper_left = [x, y, 90]
          lower_left = [x, y + inner_h, 180]
          lower_right = [x + inner_w, y + inner_h, 270]
          upper_right = [x + inner_w, y, 0]
          [
           upper_left, lower_left,
           lower_right, upper_right
          ].each do |ax, ay, start_angle|
            draw_arc(filled, ax, ay, x_diameter, y_diameter,
                     start_angle, 90, color, {:line_width => line_width})
          end
        end

        def draw_arc(filled, x, y, w, h, a1, a2, color=nil, params={})
          gc = make_gc(color, params)
          return if gc.nil?
          a1 *= 64
          a2 *= 64
          @gdk_drawable.draw_arc(gc, filled, x, y, w, h, a1, a2)
        end

        def draw_arc_by_radius(filled, x, y, r, a1, a2, color=nil, params={})
          sx = x - r
          sy = y - r
          w = r * 2
          h = r * 2
          draw_arc(filled, sx, sy, w, h, a1, a2, color, params)
        end

        def draw_polygon(filled, points, color=nil, params={})
          gc = make_gc(color, params)
          return if gc.nil?
          @gdk_drawable.draw_polygon(gc, filled, points)
        end

        def draw_layout(layout, x, y, color=nil, params={})
          gc = make_gc(color, params)
          return if gc.nil?
          @gdk_drawable.draw_layout(gc, x, y, layout)
        end

        def draw_pixbuf(pixbuf, x, y, params={})
          gc = make_gc(params['color'], params)
          return if gc.nil?
          args = [0, 0, x, y,
                  params['width'] || pixbuf.width,
                  params['height'] || pixbuf.height,
                  params['dither_mode'] || Gdk::RGB::DITHER_NORMAL,
                  params['x_dither'] || 0,
                  params['y_dither'] || 0]
          @gdk_drawable.draw_pixbuf(gc, pixbuf, *args)
        end

        private
        def init_engine_color
          @foreground = Gdk::GC.new(@gdk_drawable)
          @background = make_gc_from_string(@background_color)
        end

        # this method is no longer need. the reason that
        # this isn't removed is only for my memo.
        def set_mask(gc, x, y, mask)
          clip_mask = gc.clip_mask
          clip_origin = gc.clip_origin
          gc.clip_mask = mask
          gc.set_clip_origin(x, y)
          result = yield(gc)
          gc.clip_mask = clip_mask
          gc.set_clip_origin(*clip_origin)
          result
        end

        def internal_make_gc(color)
          if color.nil?
            Gdk::GC.new(@gdk_drawable)
          elsif color.is_a?(String)
            make_gc_from_string(color)
          elsif color.is_a?(Gdk::Color)
            make_gc_from_gdk_color(color)
          elsif color.is_a?(Renderer::Color)
            make_gc_from_gdk_color(color.to_gdk_color)
          else
            color
          end
        end

        def make_gc_from_gdk_color(color)
          gc = Gdk::GC.new(@gdk_drawable)
          gc.set_rgb_fg_color(color)
          gc
        end

        def make_gc_from_string(str)
          color = Color.parse(str).to_gdk_color
          make_gc_from_gdk_color(color)
        end

        def make_gc(color, params={})
          return nil if params[:pattern] and color.nil?
          gc = internal_make_gc(color)
          gc.set_line_attributes(get_line_width(params, 1),
                                 Gdk::GC::LINE_SOLID,
                                 Gdk::GC::CAP_ROUND,
                                 Gdk::GC::JOIN_ROUND)
          gc
        end
      end
    end
  end
end
