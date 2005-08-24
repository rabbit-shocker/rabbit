require "rabbit/rabbit"

require "rabbit/renderer/base"
require "rabbit/utils"

module Rabbit
  module Renderer
    
    class PixmapBase
      include Base
      include ScreenInfo

      @@color_table = {}
  
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
          drawable.draw_rectangle(@background, true, 0, 0, width, height)
        end
        yield
      end
      
      def draw_line(x1, y1, x2, y2, color=nil)
        gc = make_gc(color)
        drawable.draw_line(gc, x1, y1, x2, y2)
      end
      
      def draw_rectangle(filled, x1, y1, x2, y2, color=nil)
        gc = make_gc(color)
        drawable.draw_rectangle(gc, filled, x1, y1, x2, y2)
      end
      
      def draw_arc(filled, x, y, w, h, a1, a2, color=nil)
        gc = make_gc(color)
        drawable.draw_arc(gc, filled, x, y, w, h, a1 * 64, a2 * 64)
      end
      
      def draw_circle(filled, x, y, w, h, color=nil)
        draw_arc(filled, x, y, w, h, 0, 360, color)
      end
      
      def draw_polygon(filled, points, color=nil)
        gc = make_gc(color)
        drawable.draw_polygon(gc, filled, points)
      end
      
      def draw_layout(layout, x, y, color=nil)
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
        w, h = layout.size.collect {|x| x / Pango::SCALE}
        [layout, w, h]
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

      @@depth = nil
      private
      def depth
        if @@depth.nil?
          @@depth = screen_depth
        end
        @@depth
      end
      
      def can_create_pixbuf?
        true
      end

      def drawable
        @pixmap
      end

      def init_pixmap(slide, simulation)
        if simulation
          @pixmap = Gdk::Pixmap.new(nil, width, height, depth)
          @pixmaps[slide] = @pixmap
        end
      end
      
      def init_drawable
        @pixmap = Gdk::Pixmap.new(nil, 1, 1, depth)
      end
      
      def init_color
        @foreground = Gdk::GC.new(@pixmap)
        @background = Gdk::GC.new(@pixmap)
        @background.set_foreground(make_color("white"))
      end
      
      def make_gc(color, default_is_foreground=true)
        if color.nil?
          if default_is_foreground
            @foreground
          else
            @background
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

    end
  end
end
