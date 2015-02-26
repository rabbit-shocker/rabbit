require "rabbit/renderer/engine/cairo"
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
          init_context(create_context)
        end

        def post_print(canceled)
          return if canceled
          @context.target.finish
        end

        def pre_parse
        end

        def post_parse
        end

        def post_apply_theme
        end

        def post_move(old_index, index)
        end

        def post_move_in_slide(old_index, index)
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
          default_width_mm = 360
          default_height_mm = 270
          @page_width = @paper_width || default_width_mm
          @page_height = @paper_height || default_height_mm
        end

        def init_color
          super
          @foreground = make_color("black")
          @background = make_color(@background_color)
        end

        def init_dpi
          super
          @x_dpi = 300
          @y_dpi = 300
        end

        def create_context(output=nil)
          surface = find_surface(filename, output)
          surface.set_fallback_resolution(@x_dpi, @y_dpi)
          ::Cairo::Context.new(surface)
        end

        def create_pango_context
          context = create_context(StringIO.new).create_pango_layout.context
          set_font_resolution(context)
          context
        end

        def find_surface(filename, output=nil)
          args = [output || filename, @page_width, @page_height]
          case File.extname(filename)
          when /\.ps/i
            ::Cairo::PSSurface.new(*args)
          when /\.pdf/i
            ::Cairo::PDFSurface.new(*args)
          when /\.svg/i
            surface = ::Cairo::SVGSurface.new(*args)
            surface.restrict_to_version(::Cairo::SVG_VERSION_1_2)
            surface
          when /\.cs/i
            args[0] = ::Cairo::ScriptDevice.new(args[0])
            ::Cairo::ScriptSurface.new(*args)
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
