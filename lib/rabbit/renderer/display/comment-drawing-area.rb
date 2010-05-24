require 'forwardable'

require 'rabbit/renderer/base'
require 'rabbit/renderer/pixmap'
require 'rabbit/renderer/display/drawing-area-base'

module Rabbit
  module Renderer
    module Display
      class CommentDrawingArea
        include DrawingAreaBase

        extend Forwardable

        def_delegators(:@pixmap, :foreground, :background)
        def_delegators(:@pixmap, :foreground=, :background=)
        def_delegators(:@pixmap, :background_image, :background_image=)

        def_delegators(:@pixmap, :prepare_renderer, :finish_renderer)

        def_delegators(:@pixmap, :draw_slide, :draw_line, :draw_rectangle)
        def_delegators(:@pixmap, :draw_arc, :draw_circle, :draw_layout)
        def_delegators(:@pixmap, :draw_arc_by_radius, :draw_circle_by_radius)
        def_delegators(:@pixmap, :draw_pixbuf, :draw_polygon)
        def_delegators(:@pixmap, :draw_rounded_rectangle)

        def_delegators(:@pixmap, :draw_cube, :draw_sphere, :draw_cone)
        def_delegators(:@pixmap, :draw_torus, :draw_tetrahedron)
        def_delegators(:@pixmap, :draw_octahedron, :draw_dodecahedron)
        def_delegators(:@pixmap, :draw_icosahedron, :draw_teapot)

        def_delegators(:@pixmap, :gl_compile, :gl_call_list)
        def_delegators(:@pixmap, :gl_supported?)

        def_delegators(:@pixmap, :filename, :filename=)

        def_delegators(:@pixmap, :x_dpi, :y_dpi)

        def_delegators(:@pixmap, :set_font_resolution)

        attr_accessor :direction

        def width
          original_height
        end

        def height
          original_width
        end

        def initialize(canvas)
          super
          clear_pixbufs
          init_pixmap(1, 1)
          @direction = :right
        end

        def clear_keys
        end

        def update_menu
        end

        def post_apply_theme
          clear_pixbufs
          super
        end

        def toggle_comment_frame
        end

        def toggle_comment_view
        end

        def showing_comment_frame?
          false
        end

        def showing_comment_view?
          false
        end

        def comment_frame_available?
          false
        end

        def comment_view_available?
          false
        end

        def update_comment(*args, &block)
        end

        def post_init_gui
        end

        def post_toggle_index_mode
          clear_pixbufs
          super
        end

        private
        def add_widget_to_window(window)
          window.add(@area)
        end

        def remove_widget_from_window(window)
          window.remove(@area)
        end

        def attach_menu(window)
          # do nothing
        end

        def detach_menu(window)
          # do nothing
        end

        def attach_key(window)
          # do nothing
        end

        def detach_key(window)
          # do nothing
        end

        def init_drawing_area
          super
          @area.can_focus = false
        end

        def init_color
          super
          @fg_gc = Gdk::GC.new(@drawable)
        end

        def init_pixmap(w=width, h=height)
          @pixmap = Renderer::Pixmap.new(@canvas, w, h)
          @pixmap.setup_event(self)
        end

        def clear_pixbuf(slide=nil)
          @pixbufs.delete(@pixmap[slide || @canvas.current_slide])
        end

        def clear_pixbufs
          @pixbufs = {}
        end

        def slide_to_pixbuf(slide)
          pixbuf = @pixmap.to_pixbuf(slide)
          pixbuf.rotate(Gdk::Pixbuf::ROTATE_CLOCKWISE)
        end

        def draw_current_slide
          slide = @canvas.current_slide
          if slide
            unless @pixbufs.has_key?(slide)
              @pixmap.width = width
              @pixmap.height = height
              @pixbufs[slide] = slide_to_pixbuf(slide)
            end
            @drawable.draw_pixbuf(@fg_gc, @pixbufs[slide],
                                  0, 0, 0, 0, -1, -1,
                                  Gdk::RGB::DITHER_NORMAL, 0, 0)
          end
        end

        def init_renderer(drawable)
          init_pixmap(*drawable.size)
        end
      end
    end
  end
end
