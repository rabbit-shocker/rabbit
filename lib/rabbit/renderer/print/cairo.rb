require "cairo"

require "rabbit/renderer/cairo"
require "rabbit/renderer/print/base"

module Rabbit
  module Renderer
    module Print
      class Cairo
        include Base
        include Renderer::Cairo
        
        class << self
          def priority
            100
          end
        end
        
        def initialize(canvas)
          super
        end

        def pre_print(slide_size)
          super
          init_context
        end

        def post_print
          @context.target.finish
        end
        
        def pre_parse_rd
        end
        
        def post_parse_rd
        end
        
        def post_apply_theme
        end
        
        def post_move(index)
        end
        
        def _draw_slide(slide, simulation)
          yield
          @context.show_page if !simulation and @show_page
        end

        def draw_slide(slide, simulation)
          _draw_slide(slide, simulation) do
            unless simulation
              _draw_background
            end
            yield
          end
        end

        def _draw_background(x=0, y=0, w=width, h=height)
          draw_rectangle(true, x, y, w, h, @background)
        end

        private
        def init_paper
          @page_width = @paper_width || A4_HEIGHT
          @page_height = @paper_height || A4_WIDTH
        end
        
        def init_color
          super
          @foreground = make_color("black")
          @background = make_color(@background_color)
        end

        def init_context
          surface = find_surface(filename)
          @context = ::Cairo::Context.new(surface)
        end

        def find_surface(filename)
          case File.extname(filename)
          when /\.ps/i
            ::Cairo::PSSurface.new(filename, @page_width, @page_height)
          when /\.pdf/i
            ::Cairo::PDFSurface.new(filename, @page_width, @page_height)
          else
            @canvas.logger.warn(_("can't find printer for %s") % filename)
            ::Cairo::PSSurface.new("default.ps", @page_width, @page_height)
          end
        end
      end
    end
  end
end
