require "gnomeprint2"

require "rabbit/renderer/base"

module Rabbit
  module Renderer

    class GnomePrint

      include Base
      
      Color = Struct.new(:red, :green, :blue)

      class Color

        COLOR_NORMALIZE = 65536 / 256

        class << self
          def new_from_gdk_color(color)
            red = color.red / COLOR_NORMALIZE
            green = color.green / COLOR_NORMALIZE
            blue = color.blue / COLOR_NORMALIZE
            new(red, green, blue)
          end
        end
        
        def to_s
          "#%02X%02X%02X" % [red, green, blue]
        end

        def white?
          red == 255 and green == 255 and blue == 255
        end
      end
      
      attr_writer :foreground, :background, :background_image
      attr_reader :width, :height
      attr_accessor :filename
      
      def initialize(canvas)
        super
        @filename = nil
        @background_image = nil
        init_job
      end

      def print
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
      
      def post_parse_rd
        update_filename
        @job.close
      end

      
      def index_mode_on
      end
      
      def index_mode_off
      end


      def draw_page
        # @context.begin_page(@canvas.page_title) do
        @context.begin_page do
          draw_background
          yield
        end
      end
      
      def draw_line(x1, y1, x2, y2, color=nil)
        x1, y1 = from_screen(x1, y1)
        x2, y2 = from_screen(x2, y2)
        color = make_color(color)
        @context.save do
          set_color(color)
          @context.line_stroked(x1, y1, x2, y2)
        end
      end
      
      def draw_rectangle(filled, x1, y1, x2, y2, color=nil)
        x1, y1 = from_screen(x1, y1)
        y1 -= y2
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
      
      # can't draw ellipse
      def draw_arc(filled, x, y, w, h, a1, a2, color=nil)
        x, y = from_screen(x, y)
        color = make_color(color)
        @context.save do
          set_color(color)
          radius = w / 2
          @context.arc_to(x + radius, y - radius, radius, a1, a2, 0)
          if filled
            @context.fill
          else
            @context.stroke
          end
        end
      end
      
      def draw_circle(filled, x, y, w, h, color=nil)
        draw_arc(filled, x, y, w, h, 0, 359, color)
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
      

      def make_color(color, default_is_foreground=true)
        if color.nil?
          if default_is_foreground
            @foreground
          else
            @background
          end
        else
          Color.new_from_gdk_color(Gdk::Color.parse(color))
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
      
      private
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
        @width = @config[Gnome::PrintConfig::KEY_PAPER_WIDTH, :double]
        @height = @config[Gnome::PrintConfig::KEY_PAPER_HEIGHT, :double]
      end

      def init_colors
        @foreground = make_color("black")
        @background = make_color("white")
      end
      

      def from_screen(x, y)
        [x, @height - y]
      end
      
      def set_color(color)
        @context.set_rgb_color(color.red, color.green, color.blue)
      end


      def draw_background
        unless @background.white?
          draw_rectangle(true, 0, 0, @width, @height, @background.to_s)
        end
        if @background_image
          draw_pixbuf(@background_image, 0, 0)
        end
      end
      

      def update_filename
        filename = @filename || "#{GLib.filename_from_utf8(@canvas.title)}.ps"
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

      def create_dummy_pango_layout
        @context.create_layout
      end
      
    end
    
  end
end

