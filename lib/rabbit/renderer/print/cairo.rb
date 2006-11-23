require "rabbit/utils"

Rabbit::Utils.require_safe "rabbit/renderer/engine/cairo"

if !(Cairo.const_defined?(:PSSurface) or
     Cairo.const_defined?(:PDFSurface) or
     Cairo.const_defined?(:SVGSurface))
  raise LoadError
end

require "rabbit/renderer/print/base"

module Rabbit
  module Renderer
    module Print
      class Cairo
        include Engine::Cairo
        include Base
        
        class << self
          def priority
            if (::Cairo::VERSION <=> [1, 2, 0]) >= 0
              100
            else
              -100
            end
          end
        end
        
        def initialize(canvas)
          super
          init_paper
          init_color
        end

        def pre_print(slide_size)
          super
          init_context
        end

        def post_print(canceled)
          return if canceled
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
        
        def internal_draw_slide(slide, simulation)
          yield
          @context.show_page if !simulation and @show_page
        end

        def internal_clip_slide(x=0, y=0, w=width, h=height)
          x, y = from_screen(x, y)
          @context.rectangle(x, y, w, h)
          @context.clip
        end

        def internal_draw_background(x=0, y=0, w=width, h=height)
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
          surface.set_fallback_resolution(@x_dpi, @y_dpi)
          @context = ::Cairo::Context.new(surface)
          super
        end

        def find_surface(filename)
          args = [filename, @page_width, @page_height]
          case File.extname(filename)
          when /\.ps/i
            ::Cairo::PSSurface.new(*args)
          when /\.pdf/i
            ::Cairo::PDFSurface.new(*args)
          when /\.svg/i
            surface = ::Cairo::SVGSurface.new(*args)
            surface.restrict_to_version(::Cairo::SVG_VERSION_1_2)
            surface
          else
            @canvas.logger.warn(_("can't find printer for %s") % filename)
            args[0] = "default.ps"
            ::Cairo::PSSurface.new(*args)
          end
        end
      end
    end
  end
end
