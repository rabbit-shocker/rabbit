require "gnomeprint2"

require "rabbit/renderer/base"

module Rabbit
  module Renderer

    class GnomePrint

      Color = Struct.new(:red, :green, :blue)
      COLOR_NORMALIZE = 65536 / 256

      attr_writer :foreground, :background
      
      def initialize(canvas)
        @canvas = canvas
        @font_families = nil
        init_job
      end

      def print
        @job.close
        @job.print
      end
      
      def attach_to(window)
      end
    
      def print_out_filename=(filename)
        @job.print_to_file(filename)
        update_printer(filename)
      end

      def width
        @config[Gnome::PrintConfig::KEY_PAPER_WIDTH, :double]
      end
      
      def height
        @config[Gnome::PrintConfig::KEY_PAPER_HEIGHT, :double]
      end

      def destroy
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
      
      def redraw
      end
      
      
      def post_parse_rd
      end
      
      def index_mode_on
      end
      
      def index_mode_off
      end

      def post_toggle_index_mode
      end
      
      def font_families
        if @font_families.nil? or @font_families.empty?
          layout = @area.create_pango_layout("")
          @font_families = layout.context.list_families
        end
        @font_families
      end

      def post_parse_rd
        update_title
      end
      
      def draw_page
        # @context.begin_page(@canvas.page_title)
        @context.begin_page
        draw_background
        result = yield
        @context.show_page
        result
      end
      
      def draw_line(x1, y1, x2, y2, color=nil)
        x1, y1 = from_screen(x1, y1)
        x2, y2 = from_screen(x2, y2)
        color = make_color(color)
        @context.save do
          set_color(color)
          @context.new_path
          @context.move_to(x1, y1)
          @context.line_to(x2, y2)
          @context.stroke
          # @context.line_stroked(x1, y1, x2, y2)
        end
      end
      
      def draw_rectangle(filled, x1, y1, x2, y2, color=nil)
        x1, y1 = from_screen(x1, y1)
        # x2, y2 = from_screen(x2, y2)
        color = make_color(color)
        @context.save do
          set_color(color)
          if filled
            @context.rect_filled(x1, y1, x2, y2)
          else
            @context.rect_stroked(x1, y1, x2, y2)
          end
        end
      end
      
      def draw_arc(filled, x, y, w, h, a1, a2, color=nil)
        x, y = from_screen(x, y)
        color = make_color(color)
        @context.save do
          set_color(color)
          @context.arc_to(x, y, w / h, a1, a2, 0)
          @context.stroke
          @context.fill if filled
        end
      end
      
      def draw_circle(filled, x, y, w, h, color=nil)
        draw_arc(filled, x, y, w, h, 0, 360 * 64, color)
      end
      
      def draw_layout(layout, x, y, color=nil)
        x, y = from_screen(x, y)
        color = make_color(color)
        @context.save do
          set_color(color)
          @context.move_to(x, y)
          @context.layout(layout)
        end
      end
      
      def draw_pixbuf(pixbuf, x, y, params={})
        x, y = from_screen(x, y)
        color = make_color(params['color'])
        args = [pixbuf.pixels,
          params['width'] || pixbuf.width,
          params['height'] || pixbuf.height,
          pixbuf.rowstride]
        @context.save do
          @context.translate(x, y)
          @context.scale(144, 144)
          if pixbuf.has_alpha?
            @context.rgba_image(*args)
          else
            @context.rgb_image(*args)
          end
        end
      end
      
      def make_color(color, default_is_foreground=true)
        if color.nil?
          if default_is_foreground
            @foreground
          else
            @background
          end
        else
          color = Gdk::Color.parse(color)
          red = color.red / COLOR_NORMALIZE
          green = color.green / COLOR_NORMALIZE
          blue = color.blue / COLOR_NORMALIZE
          Color.new(red, green, blue)
        end
      end

      def make_layout(text)
        attrs, text = Pango.parse_markup(text)
        layout = @context.create_layout
        layout.text = text
        layout.set_attributes(attrs)
        layout.context_changed
        w, h = layout.size.collect {|x| x / Pango::SCALE}
        [layout, w, h]
      end
      
      def set_color(color)
        @context.set_rgb_color(color.red, color.green, color.blue)
      end

      private
      def draw_background
        if @background.red == 255 and
            @background.green == 255 and
            @background.blue == 255
        else
          color = "#%2d%2d%2d" % [@background.red, @background.green, @background.blue]
          draw_rectangle(true, 0, height, width, 0, color)
        end
      end
      
      def from_screen(x, y)
        [x, height - y]
      end
      
      def init_job
        @job = Gnome::PrintJob.new
        @context = @job.context
        @config = @job.config
        init_printers
        init_paper
        init_colors
      end

      def init_printers
        @printers = Gnome::PrintGPA.printers
      end
      
      def init_paper
        paper = Gnome::PrintPaper.get_by_name("A4")
        @config[Gnome::PrintConfig::KEY_PAPER_WIDTH] = paper.height
        @config[Gnome::PrintConfig::KEY_PAPER_HEIGHT] = paper.width
      end

      def init_colors
        @foreground = make_color("black")
        @background = make_color("white")
      end
      
      def update_title
        @config[Gnome::PrintConfig::KEY_DOCUMENT_NAME] = @canvas.title
      end

      def update_printer(filename)
        printer = find_printer(filename)
        if printer
          @config["Printer"] = printer.name
        else
          @canvas.logger.warn(_("can't find printer for %s") % filename)
        end
      end
      
      def find_printer(filename)
        case File.extname(filename)
        when /\.ps/i
          @printers.find do |printer|
            /Postscript/i =~ printer.description
          end
        when /\.pdf/i
          @printers.find do |printer|
            /PDF/i =~ printer.description
          end
        else
          nil
        end
      end
      
    end
    
  end
end

