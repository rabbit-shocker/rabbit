require "rabbit/rabbit"

require "rabbit/renderer/base"
require "rabbit/utils"

module Rabbit
  module Renderer
    module Pixmap
      module Base
        include Renderer::Base

        @@depth = nil
        
        attr_accessor :width, :height, :pango_context
        
        attr_accessor :filename
        
        def initialize(canvas, width=nil, height=nil)
          super(canvas)
          @width = width
          @height = height
          @filename = nil
          init_dummy_pixmap
          init_color
          @pango_context = nil
        end

        def post_apply_theme
        end

        def post_move(old_index, index)
        end

        def post_move_in_slide(old_index, index)
        end

        def pre_parse
        end

        def post_parse
        end

        def index_mode_on
        end

        def index_mode_off
        end

        def pre_toggle_index_mode
        end

        def post_toggle_index_mode
        end

        def make_layout(text)
          attrs, text = Pango.parse_markup(text)
          layout = Pango::Layout.new(@pango_context || create_pango_context)
          layout.text = text
          layout.set_attributes(attrs)
          layout
        end

        def to_pixbuf(slide)
          slide.draw(@canvas)
          Utils.drawable_to_pixbuf(@pixmap)
        end

        def create_pango_context
          context = Gtk::Invisible.new.create_pango_context
          set_font_resolution(context)
          context
        end

        def pre_to_pixbuf(slide_size)
        end

        def to_pixbufing(i)
          Utils.process_pending_events
          true
        end

        def post_to_pixbuf(canceled)
        end

        def draw_slide(slide, simulation)
          init_pixmap(slide, simulation)
          super
        end

        private
        def init_color
          super
          init_engine_color
        end

        def depth
          @@depth ||= ScreenInfo.screen_depth
        end

        def off_screen_renderer?
          true
        end

        def init_dpi
          @x_dpi = ScreenInfo.screen_x_resolution
          @y_dpi = ScreenInfo.screen_y_resolution
        end

        def init_pixmap(slide, simulation)
          if simulation
            if @pixmap.nil? or @pixmap.size != [@width, @height]
              @pixmap = Gdk::Pixmap.new(nil, @width, @height, depth)
              init_gl_capability(@pixmap)
            end
            init_renderer(@pixmap)
          end
        end

        def init_dummy_pixmap
          @pixmap = Gdk::Pixmap.new(nil, 1, 1, depth)
          init_gl_capability(@pixmap)
          init_renderer(@pixmap)
        end
      end
    end
  end
end
