module Rabbit
  module Utils
    def to_class_name(name)
      name.gsub(/(?:\A|_|\-)([a-z])/) do |x|
        $1.upcase
      end
    end

    module Canvas
      
      @@color_table = {}

      def draw_line(canvas, x1, y1, x2, y2, color=nil)
        gc = make_gc(canvas, color)
        canvas.drawable.draw_line(gc, x1, y1, x2, y2)
      end

      def draw_rectangle(canvas, filled, x1, y1, x2, y2, color=nil)
        gc = make_gc(canvas, color)
        canvas.drawable.draw_rectangle(gc, filled, x1, y1, x2, y2)
      end

      def draw_arc(canvas, filled, x, y, w, h, a1, a2, color=nil)
        gc = make_gc(canvas, color)
        canvas.drawable.draw_arc(gc, filled, x, y, w, h, a1, a2)
      end

      def draw_circle(canvas, filled, x, y, w, h, color=nil)
        draw_arc(canvas, filled, x, y, w, h, 0, 360 * 64, color)
      end

      def draw_layout(canvas, layout, x, y, color=nil)
        gc = make_gc(canvas, color)
        canvas.drawable.draw_layout(gc, x, y, layout)
      end

      def draw_pixbuf(canvas, pixbuf, x, y, params={})
        gc = make_gc(canvas, params['color'])
        args = [0, 0, x, y,
                params['width'] || pixbuf.width,
                params['height'] || pixbuf.height,
                params['dither_mode'] || Gdk::RGB::DITHER_NORMAL,
                params['x_dither'] || 0,
                params['y_dither'] || 0]
        canvas.drawable.draw_pixbuf(gc, pixbuf, *args)
      end

      def make_color(canvas, color, default_is_foreground=true)
        make_gc(canvas, color, default_is_foreground).foreground
      end

      def make_gc(canvas, color, default_is_foreground=true)
        if color.nil?
          if default_is_foreground
            canvas.foreground
          else
            canvas.background
          end
        else
          make_gc_from_rgb_string(canvas, color)
        end
      end

      def make_gc_from_rgb_string(canvas, str)
        r, g, b = str.scan(/[a-fA-F\d]{4,4}/).collect{|x| x.hex}
        make_gc_from_rgb(canvas, r, g, b)
      end

      def make_gc_from_rgb(canvas, r, g, b)
        gc = Gdk::GC.new(canvas.drawable)
        if @@color_table.has_key?([r, g, b])
          color = @@color_table[[r, g, b]]
        else
          color = Gdk::Color.new(r, g, b)
          colormap = Gdk::Colormap.system
          colormap.alloc_color(color, false, true)
          @@color_table[[r, g, b]] = color
        end
        gc.set_foreground(color)
        gc
      end

      def make_layout(canvas, text)
        attrs, text = Pango.parse_markup(text)
        layout = canvas.drawing_area.create_pango_layout(text)
        layout.set_attributes(attrs)
        w, h = layout.size.collect {|x| x / Pango::SCALE}
        [layout, w, h]
      end
    end
    
  end
end
