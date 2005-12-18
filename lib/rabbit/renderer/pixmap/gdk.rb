require "rabbit/rabbit"

require "rabbit/renderer/base"
require "rabbit/utils"

module Rabbit
  module Renderer
    class PixmapGdk
      include Base
      include ScreenInfo

      @@color_table = {}

      class << self
        def priority
          0
        end
      end
      
      attr_accessor :width, :height, :pango_context
      
      attr_accessor :filename
      
      def initialize(canvas, width=nil, height=nil)
        super(canvas)
        @width = width
        @height = height
        @filename = nil
        @pixmaps = {}
        @pango_context = create_pango_context
        init_drawable
        init_color
        clear_pixmaps
      end

      def has_key?(slide)
        @pixmaps.has_key?(slide)
      end
      
      def [](slide)
        @pixmaps[slide]
      end

      def []=(slide, pixmap)
        @pixmaps[slide] = pixmap
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
      
      def post_apply_theme
        clear_pixmaps
      end

      def post_move(index)
      end

      def pre_parse_rd
      end
      
      def post_parse_rd
        clear_pixmaps
      end
      
      def index_mode_on
      end
      
      def index_mode_off
      end
      
      def post_toggle_index_mode
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
      
      def draw_circle(filled, x, y, w, h, color=nil, params={})
        draw_arc(filled, x, y, w, h, 0, 360, color, params)
      end
      
      def draw_circle_by_radius(filled, x, y, r, color=nil, params={})
        draw_arc_by_radius(filled, x, y, r, 0, 360, color, params)
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
        make_gc(color, default_is_foreground).foreground
      end

      def to_rgb(color)
        color = make_color(color) unless color.is_a?(Gdk::Color)
        color = Gdk::Colormap.system.query_color(color.pixel)
        [color.red, color.green, color.blue]
      end
      
      def make_layout(text)
        attrs, text = Pango.parse_markup(text)
        layout = Pango::Layout.new(@pango_context)
        layout.text = text
        layout.set_attributes(attrs)
        layout
      end

      def to_pixbuf(slide)
        drawable = @pixmaps[slide]
        args = [drawable.colormap, drawable, 0, 0, width, height]
        Gdk::Pixbuf.from_drawable(*args)
      end

      def clear_pixmap(slide=nil)
        dirty
        @pixmaps.delete(slide || @canvas.current_slide)
        clean_if_dirty
      end

      def clear_pixmaps
        dirty
        @pixmaps = {}
        clean_if_dirty
      end

      def clean
        super
        GC.start
      end

      def create_pango_context
        Gtk::Invisible.new.create_pango_context
      end
      
      def pre_to_pixbuf(slide_size)
      end

      def to_pixbufing(i)
      end
      
      def post_to_pixbuf
      end

      def clear_theme
        init_drawable
        init_color
        clear_pixmaps
        super
      end
      
      @@depth = nil
      private
      def depth
        if @@depth.nil?
          @@depth = screen_depth
        end
        @@depth
      end
      
      def off_screen_renderer?
        true
      end

      def drawable
        @pixmap
      end

      def init_pixmap(slide, simulation)
        if simulation
          @pixmap = Gdk::Pixmap.new(nil, @width, @height, depth)
          @pixmaps[slide] = @pixmap
        end
      end
      
      def init_drawable
        @pixmap = Gdk::Pixmap.new(nil, 1, 1, depth)
      end
      
      def init_color
        @background_color = "white"
        @foreground = Gdk::GC.new(@pixmap)
        @background = make_gc_from_string(@background_color)
      end

      def make_gc(color, default_is_foreground=true)
        if color.nil?
          if default_is_foreground
            Gdk::GC.new(@pixmap)
          else
            make_gc_from_string(@background_color)
          end
        elsif color.is_a?(String)
          make_gc_from_string(color)
        else
          color
        end
      end

      def make_gc_from_string(str)
        gc = Gdk::GC.new(@pixmap)
        if @@color_table.has_key?(str)
          color = @@color_table[str]
        else
          color = Gdk::Color.parse(str)
          colormap = Gdk::Colormap.system
          unless colormap.alloc_color(color, false, true)
            raise CantAllocateColorError.new(str)
          end
          @@color_table[str] = color
        end
        gc.set_foreground(color)
        gc
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
