require "forwardable"

require 'rabbit/renderer/kernel'
require "rabbit/renderer/print/base"
require "rabbit/renderer/print/layout"

module Rabbit
  module Renderer
    module Print
      class Multiple
        include Base
        include Kernel
        extend Forwardable
      
        def_delegators(:@canvas, :current_index)
        
        def_delegators(:@print, :filename, :filename=)
        def_delegators(:@print, :foreground=, :background=)
        def_delegators(:@print, :background_image=)
        
        def_delegators(:@print, :page_width, :page_width=)
        def_delegators(:@print, :page_height, :page_height=)
        def_delegators(:@print, :width, :height)
        
        def_delegators(:@print, :pre_print, :post_print, :post_apply_theme)
        def_delegators(:@print, :post_move, :post_move_in_slide, :post_iconify)
        def_delegators(:@print, :post_fullscreen, :post_unfullscreen)
        def_delegators(:@print, :pre_parse, :post_parse)
        def_delegators(:@print, :post_toggle_index_mode)
        def_delegators(:@print, :index_mode_on, :index_mode_off)

        def_delegators(:@print, :make_color, :make_layout)
        def_delegators(:@print, :create_pango_context)
        
        def_delegators(:@print, :clear_theme)

        def_delegators(:@print, :rsvg_available?, :poppler_available?)
        def_delegators(:@print, :rsvg_available?, :poppler_available?)

        def initialize(canvas)
          @print = Print.new(canvas)
          super
        end
        
        def width
          @slide_width ||= @layout.slide_width
        end

        def height
          @slide_height ||= @layout.slide_height
        end

        def page_margin_left=(margin)
          super
          @print.page_margin_left = margin
        end

        def page_margin_right=(margin)
          super
          @print.page_margin_right = margin
        end
        
        def page_margin_top=(margin)
          super
          @print.page_margin_top = margin
        end
        
        def page_margin_bottom=(margin)
          super
          @print.page_margin_bottom = margin
        end

        def margin_left=(margin)
          super
          @print.margin_left = margin
        end

        def margin_right=(margin)
          super
          @print.margin_right = margin
        end
        
        def margin_top=(margin)
          super
          @print.margin_top = margin
        end
        
        def margin_bottom=(margin)
          super
          @print.margin_bottom = margin
        end

        
        def draw_slide(slide, simulation)
          @print.show_page = need_show_page?
          @print.internal_draw_slide(slide, simulation) do
            if simulation
              yield
            else
              @print.save_context do
                x, y = normalize(0, 0)
                @print.internal_clip_slide(x, y, width, height)
                @print.internal_draw_background(x, y, width, height)
                yield
                draw_rectangle(false, 0, 0, width, height, @black)
              end
            end
          end
        end

        def slides_per_page=(slides)
          super
          update_layout
        end

        def draw_line(x1, y1, x2, y2, color=nil, params={})
          x1, y1 = normalize(x1, y1)
          x2, y2 = normalize(x2, y2)
          @print.draw_line(x1, y1, x2, y2, color, params)
        end

        def draw_rectangle(filled, x, y, w, h, color=nil, params={})
          x, y = normalize(x, y)
          @print.draw_rectangle(filled, x, y, w, h, color, params)
        end

        def draw_rounded_rectangle(filled, x, y, w, h, radius, color=nil,
                                   params={})
          x, y = normalize(x, y)
          @print.draw_rounded_rectangle(filled, x, y, w, h, radius,
                                        color, params)
        end

        def draw_arc(filled, x, y, w, h, a1, a2, color=nil, params={})
          x, y = normalize(x, y)
          @print.draw_arc(filled, x, y, w, h, a1, a2, color, params)
        end

        def draw_arc_by_radius(filled, x, y, r, a1, a2, color=nil, params={})
          x, y = normalize(x, y)
          @print.draw_arc_by_radius(filled, x, y, r, a1, a2, color, params)
        end

        def draw_layout(layout, x, y, color=nil, params={})
          x, y = normalize(x, y)
          @print.draw_layout(layout, x, y, color, params)
        end

        def draw_pixbuf(pixbuf, x, y, params={})
          x, y = normalize(x, y)
          @print.draw_pixbuf(pixbuf, x, y, params)
        end

        def draw_rsvg_handle(handle, x, y, params={})
          x, y = normalize(x, y)
          @print.draw_rsvg_handle(handle, x, y, params)
        end

        def draw_poppler_page(handle, x, y, params={})
          x, y = normalize(x, y)
          @print.draw_poppler_page(handle, x, y, params)
        end

        private
        def init_paper
          @print.paper_width = A4_WIDTH
          @print.paper_height = A4_HEIGHT
        end

        def init_color
          @white = make_color("white")
          @black = make_color("black")
        end

        def update_layout
          if @slides_per_page
            @layout = LayoutBase.make_layout(self)
          else
            @layout = nil
          end
          @slide_width = nil
          @slide_height = nil
        end

        def normalize_x(x)
          @layout.normalize_x(x)
        end
        alias nx normalize_x

        def normalize_y(y)
          @layout.normalize_y(y)
        end
        alias ny normalize_y

        def normalize(x, y)
          [nx(x), ny(y)]
        end

        def need_show_page?
          @canvas.last_slide? or ((current_index + 1) % @slides_per_page).zero?
        end
      end
    end
  end
end
