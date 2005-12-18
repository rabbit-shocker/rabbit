require "rabbit/rabbit"

require "rabbit/renderer/base"
require "rabbit/utils"

module Rabbit
  module Renderer
    module PixmapBase
      include Base
      include ScreenInfo

      @@color_table = {}
      @@depth = nil
      
      attr_accessor :width, :height, :pango_context
      
      attr_accessor :filename
      
      def initialize(canvas, width=nil, height=nil)
        super(canvas)
        @width = width
        @height = height
        @filename = nil
        @pixmaps = {}
        init_drawable
        init_color
        clear_pixmaps
        @pango_context = create_pango_context
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
      
      def make_layout(text)
        attrs, text = Pango.parse_markup(text)
        layout = Pango::Layout.new(@pango_context)
        layout.text = text
        layout.set_attributes(attrs)
        layout
      end

      def make_gdk_color(color, default_is_foreground=true)
        make_gc(color, default_is_foreground).foreground
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

      def init_color
        @background_color = "white"
      end
    end
  end
end
