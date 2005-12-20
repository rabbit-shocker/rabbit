require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    module Pixmap
      class GDK
        include Base
        
        class << self
          def priority
            0
          end
        end
        
        def foreground=(color)
          @foreground.set_foreground(color)
        end
        
        def background=(color)
          @background.set_foreground(color)
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
        
        def draw_slide(slide, simulation)
          init_pixmap(slide, simulation)
          unless simulation
            draw_rectangle(true, 0, 0, width, height, @background)
          end
          yield
        end
        
        def draw_line(x1, y1, x2, y2, color=nil, params={})
          gc = make_gc(color)
          line_width = get_line_width(params, 1) / 2
          if x1 == x2
            line_width.times do |i|
              x_delta = i + 1
              drawable.draw_line(gc, x1 - x_delta, y1, x2 - x_delta, y2)
              drawable.draw_line(gc, x1 + x_delta, y1, x2 + x_delta, y2)
            end
          elsif y1 == y2
            line_width.times do |i|
              y_delta = i + 1
              drawable.draw_line(gc, x1, y1 - y_delta, x2, y2 - y_delta)
              drawable.draw_line(gc, x1, y1 + y_delta, x2, y2 + y_delta)
            end
          else
            slope = (y2 - y1).to_f / (x2 - x1).to_f
            theta = Math.atan(slope.abs)
            y_delta = Math.sin(theta) * line_width
            0.step(y_delta, y_delta > 0 ? 1 : -1) do |i|
              x_delta = slope * i
              drawable.draw_line(gc, x1 - x_delta, y1 + i, x2 - x_delta, y2 + i)
              drawable.draw_line(gc, x1 + x_delta, y1 - i, x2 + x_delta, y2 - i)
            end
          end
          drawable.draw_line(gc, x1, y1, x2, y2)
        end
        
        def draw_rectangle(filled, x, y, w, h, color=nil, params={})
          gc = make_gc(color)
          unless filled
            line_width = get_line_width(params, 1) / 2
            line_width.times do |i|
              delta = i + 1
              double_delta = delta * 2
              drawable.draw_rectangle(gc, false,
                                      x - delta, y - delta,
                                      w + double_delta, h + double_delta)
              drawable.draw_rectangle(gc, false,
                                      x + delta, y + delta,
                                      w - double_delta, h - double_delta)
            end
          end
          drawable.draw_rectangle(gc, filled, x, y, w, h)
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
          gc = make_gc(color)
          a1 *= 64
          a2 *= 64
          line_width = get_line_width(params)
          if filled or line_width.nil? or line_width == 1
            drawable.draw_arc(gc, filled, x, y, w, h, a1, a2)
          else
            half_line_width = line_width / 2 + 1
            nx = x - half_line_width
            ny = y - half_line_width
            nw = w + half_line_width * 2
            nh = w + half_line_width * 2
            mask = Gdk::Pixmap.new(nil, nw, nh, 1)
            xor_gc = Gdk::GC.new(mask)
            xor_gc.set_function(Gdk::GC::INVERT)
            clear_gc = Gdk::GC.new(mask)
            clear_gc.set_function(Gdk::GC::SET)
            mask.draw_rectangle(clear_gc, true, 0, 0, nw, nh)
            masked_w = nw - line_width * 2
            masked_h = nh - line_width * 2
            mask.draw_arc(xor_gc, true, line_width, line_width,
                          masked_w, masked_h, a1, a2)
            set_mask(gc, nx, ny, mask) do |gc|
              drawable.draw_arc(gc, true, nx, ny, nw, nh, a1, a2)
            end
          end
        end
        
        def draw_arc_by_radius(filled, x, y, r, a1, a2, color=nil, params={})
          sx = x - r
          sy = y - r
          w = r * 2
          h = r * 2
          draw_arc(filled, sx, sy, w, h, a1, a2, color, params)
        end
        
        def draw_polygon(filled, points, color=nil, params={})
          gc = make_gc(color)
          drawable.draw_polygon(gc, filled, points)
        end
        
        def draw_layout(layout, x, y, color=nil, params={})
          gc = make_gc(color)
          drawable.draw_layout(gc, x, y, layout)
        end
        
        def draw_pixbuf(pixbuf, x, y, params={})
          gc = make_gc(params['color'])
          args = [0, 0, x, y,
                  params['width'] || pixbuf.width,
                  params['height'] || pixbuf.height,
                  params['dither_mode'] || Gdk::RGB::DITHER_NORMAL,
                  params['x_dither'] || 0,
                  params['y_dither'] || 0]
          drawable.draw_pixbuf(gc, pixbuf, *args)
        end
        
        def make_color(color, default_is_foreground=true)
          make_gdk_color(color, default_is_foreground)
        end
        
        def to_gdk_rgb(color)
          color = make_color(color) unless color.is_a?(Gdk::Color)
          color = Gdk::Colormap.system.query_color(color.pixel)
          [color.red, color.green, color.blue]
        end
        
        def init_color
          super
          @foreground = Gdk::GC.new(@pixmap)
          @background = make_gc_from_string(@background_color)
        end
        
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
      end
    end
  end
end
