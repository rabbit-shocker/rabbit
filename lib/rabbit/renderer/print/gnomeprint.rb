require "gnomeprint2"

require "rabbit/renderer/print/base"

module Rabbit
  module Renderer
    module Print
      class GnomePrint
        include Base

        class << self
          def priority
            0
          end
        end
        
        attr_writer :foreground, :background, :background_image
        
        def initialize(canvas)
          super
          @background_image = nil
          init_job
          init_printers
          init_paper
          init_color
        end
        
        def paper_width=(value)
          super
          init_paper
        end
        
        def paper_height=(value)
          super
          init_paper
        end
        
        def pre_print(slide_size)
          super
          update_filename
          @shown_page = true
        end
        
        def post_print(canceled)
          return if canceled
          @job.close
          @job.print
        end
        
        def post_apply_theme
        end
        
        def post_move(index)
        end
        
        def post_fullscreen
        end
        
        def post_unfullscreen
        end
        
        def post_iconify
        end
        
        def post_toggle_index_mode
        end
        
        def pre_parse_rd
        end
        
        def post_parse_rd
          update_title
        end
        
        
        def index_mode_on
        end
        
        def index_mode_off
        end
        
        
        def _draw_slide(slide, simulation)
          if simulation
            yield
          else
            # @context.begin_page(slide.title) if @shown_page
            @context.begin_page if @shown_page
            yield
            if @show_page
              @context.show_page
              @shown_page = true
            else
              @shown_page = false
            end
          end
        end
        
        def draw_slide(slide, simulation)
          _draw_slide(slide, simulation) do
            unless simulation
              _draw_background
            end
            yield
          end
        end
        
        def draw_line(x1, y1, x2, y2, color=nil, params={})
          x1, y1 = from_screen(x1, y1)
          x2, y2 = from_screen(x2, y2)
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            @context.line_stroked(x1, y1, x2, y2)
          end
        end
        
        def draw_rectangle(filled, x, y, w, h, color=nil, params={})
          x, y = from_screen(x, y)
          y -= h
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            if filled
              @context.rect_filled(x, y, w, h)
            else
              @context.rect_stroked(x, y, w, h)
            end
          end
        end
        
        def draw_rounded_rectangle(filled, x, y, w, h, radius, color=nil, params={})
          unless @context.respond_to?(:rounded_rect)
            not_support_method("draw_rounded_rectangle")
          end
          
          x, y = from_screen(x, y)
          x_radius = params[:x_radius] || radius
          y_radius = params[:y_radius] || radius
          
          @context.save do
            set_color(make_color(color))
            set_line_width(get_line_width(params))
            args = [x, y, w, h, x_radius, y_radius]
            if filled
              @context.rounded_rect_filled(*args)
            else
              @context.rounded_rect_stroked(*args)
            end
          end
        end
        
        # can't draw ellipse
        def draw_arc(filled, x, y, w, h, a1, a2, color=nil, params={})
          r = w * 0.5
          draw_arc_by_radius(filled, x + w * 0.5, y + h * 0.5,
                             r, a1, a2, color, params)
        end
        
        def draw_arc_by_radius(filled, x, y, r, a1, a2, color=nil, params={})
          x, y = from_screen(x, y)
          a1, a2 = convert_angle(a1, a2)
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            args = [x, y, r, a1, a2, false]
            if filled
              @context.move_to(x, y)
              @context.arc_to(*args)
              @context.close_path
              @context.fill
            else
              @context.arc_to(*args)
              @context.stroke
            end
          end
        end
        
        def draw_polygon(filled, points, color=nil, params={})
          return if points.empty?
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            @context.move_to(*from_screen(*points.first))
            points[1..-1].each do |x, y|
              @context.line_to(*from_screen(x, y))
            end
            @context.line_to(*from_screen(*points.first))
            if filled
              @context.fill
            else
              @context.stroke
            end
          end
        end
        
        def draw_layout(layout, x, y, color=nil, params={})
          x, y = from_screen(x, y)
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            @context.move_to(x, y)
            @context.layout(layout)
          end
        end
        
        def draw_pixbuf(pixbuf, x, y, params={})
          x, y = from_screen(x, y)
          color = make_color(params['color'])
          width = params['width'] || pixbuf.width
          height = params['height'] || pixbuf.height
          args = [pixbuf.pixels, width, height, pixbuf.rowstride]
          @context.save do
            @context.translate(x, y - height)
            @context.scale(width, height)
            if pixbuf.has_alpha?
              @context.rgba_image(*args)
            else
              @context.rgb_image(*args)
            end
          end
        end
        
        def _draw_background(x=0, y=0, w=width, h=height)
          draw_rectangle(true, x, y, w, h, @background)
          if @background_image
            params = {
              "width" => [@background_image.width, w].min,
              "height" => [@background_image.height, h].min,
            }
            draw_pixbuf(@background_image, x, y, params)
          end
        end
        
        
        def make_color(color, default_is_foreground=true)
          if color.is_a?(Color)
            color
          elsif color.nil?
            if default_is_foreground
              @foreground
            else
              @background
            end
          else
            Color.parse(color)
          end
        end
        
        def make_layout(text)
          attrs, text = Pango.parse_markup(text)
          layout = @context.create_layout
          layout.text = text
          layout.set_attributes(attrs)
          layout.context_changed
          layout
        end
        
        def create_pango_context
          Gnome::PrintPango.create_context(Gnome::PrintPango.default_font_map)
        end
        
        def clear_theme
          init_job
          init_color
          @background_image = nil
        end
        
        private
        def init_job
          @job = Gnome::PrintJob.new
          @context = @job.context
          set_line_width(1)
          @config = @job.config
        end
        
        def init_printers
          @printers = Gnome::GPARoot.printers
        end
        
        def init_paper
          setup_paper
          @page_width = get_length_by_point(Gnome::PrintConfig::KEY_PAPER_WIDTH)
          @page_height = get_length_by_point(Gnome::PrintConfig::KEY_PAPER_HEIGHT)
        end
        
        def setup_paper
          pt = unit("Pt")
          if size_set?
            @config[Gnome::PrintConfig::KEY_PAPER_SIZE] = "Custom"
            @config.set(Gnome::PrintConfig::KEY_PAPER_WIDTH, @paper_width, pt)
            @config.set(Gnome::PrintConfig::KEY_PAPER_HEIGHT, @paper_height, pt)
          else
            paper = Gnome::PrintPaper.get("A4")
            @config[Gnome::PrintConfig::KEY_PAPER_SIZE] = "Custom"
            @config.set(Gnome::PrintConfig::KEY_PAPER_WIDTH, paper.height, pt)
            @config.set(Gnome::PrintConfig::KEY_PAPER_HEIGHT, paper.width, pt)
          end
        end
        
        def size_set?
          @paper_width and @paper_height
        end
        
        def get_length_by_point(key, *args)
          pt = unit("Pt")
          length, _unit = @config[key, :length]
          _unit.convert_distance(length, pt, *args)
        end
        
        def unit(abbr_name)
          Gnome::PrintUnit.get_by_abbreviation(abbr_name)
        end
        
        def init_color
          super
          @foreground = make_color("black")
          @background = make_color(@background_color)
        end
        
        
        def from_screen(x, y)
          [x + margin_page_left, invert_y(y) + margin_page_bottom]
        end
        
        def convert_angle(a1, a2)
          a2 += a1
          a2 -= 0.000001 if a2 == 360
          [a1, a2]
        end
        
        def set_color(color)
          red, green, blue, alpha = color.to_a
          @context.set_rgb_color(red, green, blue)
          @context.set_opacity(alpha) if alpha
        end
        
        def set_line_width(line_width)
          if line_width
            @context.set_line_width(line_width)
          end
        end
        
        def update_filename
          update_printer(filename)
          @job.print_to_file(filename)
          init_paper
        end
        
        def update_title
          @config[Gnome::PrintConfig::KEY_DOCUMENT_NAME] = @canvas.title
        end
        
        def update_printer(filename)
          printer = find_printer(filename)
          if printer
            @config["Printer"] = printer.id
          else
            @canvas.logger.warn(_("can't find printer for %s") % filename)
          end
        end
        
        def find_printer(filename)
          if filename[0] == ?|
            @printers.find do |printer|
              /Postscript/i =~ printer.value
            end
          else
            case File.extname(filename)
            when /\.ps/i
              @printers.find do |printer|
                /Postscript/i =~ printer.value
              end
            when /\.pdf/i
              @printers.find do |printer|
                /PDF/i =~ printer.value
              end
            else
              nil
            end
          end
        end
      end
    end
  end
end
