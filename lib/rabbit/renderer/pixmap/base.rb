require "rabbit/rabbit"

require "rabbit/renderer/base"
require "rabbit/utils"

module Rabbit
  module Renderer
    module Pixmap
      module Base
        include Renderer::Base
        include ScreenInfo
        
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
        
        def to_pixbuf(slide)
          Utils.drawable_to_pixbuf(@pixmaps[slide])
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
          true
        end
        
        def post_to_pixbuf(canceled)
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

        def init_dpi
          screen = Gdk::Screen.default
          # from xdpyinfo
          ## there are 2.54 centimeters to an inch;
          ## so there are 25.4 millimeters.
          ##
          ##     dpi = N pixels / (M millimeters / (25.4 millimeters / 1 inch))
          ##         = N pixels / (M inch / 25.4)
          ##         = N * 25.4 pixels / M inch
          @x_dpi = ((screen.width * 25.4) / screen.width_mm).round
          @y_dpi = ((screen.height * 25.4) / screen.height_mm).round
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
      end
    end
  end
end
