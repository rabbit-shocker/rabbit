require "gtk2"

require "rabbit/rabbit"
require "rabbit/gettext"

module Rabbit
  module Renderer

    module Base
      include GetText

      attr_accessor :paper_width, :paper_height, :slides_per_page
      attr_accessor :left_margin, :right_margin
      attr_accessor :top_margin, :bottom_margin
      attr_writer :left_page_margin, :right_page_margin
      attr_writer :top_page_margin, :bottom_page_margin
      
      def initialize(canvas)
        @canvas = canvas
        @font_families = nil
        @paper_width = nil
        @paper_height = nil
        @slides_per_page = nil
        @left_margin = nil
        @right_margin = nil
        @top_margin = nil
        @bottom_margin = nil
        @left_page_margin = nil
        @right_page_margin = nil
        @top_page_margin = nil
        @bottom_page_margin = nil
      end

      def left_page_margin
        @left_page_margin || 0
      end
      
      def right_page_margin
        @right_page_margin || 0
      end
      
      def top_page_margin
        @top_page_margin || 0
      end
      
      def bottom_page_margin
        @bottom_page_margin || 0
      end
      
      def font_families
        if @font_families.nil? or @font_families.empty?
          @font_families = create_pango_context.list_families
        end
        @font_families
      end

      def print(&block)
        if printable?
          do_print(&block)
        else
          canvas = make_canvas_with_printable_renderer
          pre_print(canvas.slide_size)
          canvas.print do |i|
            printing(i)
          end
          post_print
        end
      end

      def redraw
      end
      
      def each_slide_pixbuf
        if can_create_pixbuf?
          canvas = @canvas
        else
          canvas = make_canvas_with_offscreen_renderer
        end
        previous_index = canvas.current_index
        pre_to_pixbuf(canvas.slide_size)
        canvas.slides.each_with_index do |slide, i|
          to_pixbufing(i)
          canvas.move_to_if_can(i)
          slide.draw(canvas)
          yield(canvas.to_pixbuf(slide), i)
        end
        post_to_pixbuf
        canvas.move_to_if_can(previous_index)
      end
      
      def create_pango_context
        Pango::Context.new
      end
      
      def printable?
        false
      end

      private
      def can_create_pixbuf?
        false
      end
      
      def do_print(&block)
        pre_print(@canvas.slide_size)
        @canvas.slides.each_with_index do |slide, i|
          @canvas.move_to_if_can(i)
          @canvas.current_slide.draw(@canvas)
          block.call(i) if block
        end
        post_print
      end

      def make_canvas_with_renderer(renderer)
        canvas = Canvas.new(@canvas.logger, renderer)
        yield canvas
        canvas.apply_theme(@canvas.theme_name)
        @canvas.source_force_modified(true) do |source|
          canvas.parse_rd(source)
        end
        canvas.toggle_index_mode if @canvas.index_mode?
        canvas
      end
      
      def make_canvas_with_printable_renderer
        renderer = Renderer.printable_renderer(@canvas.slides_per_page)
        make_canvas_with_renderer(renderer) do |canvas|
          canvas.filename = @canvas.filename
          setup_margin(canvas)
          setup_page_margin(canvas)
          setup_paper_size(canvas)
          canvas.slides_per_page = @canvas.slides_per_page
        end
      end
      
      def make_canvas_with_offscreen_renderer
        make_canvas_with_renderer(Pixmap) do |canvas|
          canvas.width = @canvas.width
          canvas.height = @canvas.height
        end
      end

      def setup_margin(canvas)
        canvas.left_margin = @canvas.left_margin
        canvas.right_margin = @canvas.right_margin
        canvas.top_margin = @canvas.top_margin
        canvas.bottom_page_margin = @canvas.bottom_page_margin
      end

      def setup_page_margin(canvas)
        canvas.left_page_margin = @canvas.left_page_margin
        canvas.right_page_margin = @canvas.right_page_margin
        canvas.top_page_margin = @canvas.top_page_margin
        canvas.bottom_page_margin = @canvas.bottom_page_margin
      end

      def setup_paper_size(canvas)
        if @canvas.paper_width and @canvas.paper_height
          canvas.paper_width = @canvas.paper_width
          canvas.paper_height = @canvas.paper_height
        else
          canvas.paper_width = @canvas.width
          canvas.paper_height = @canvas.height
        end
      end
      
    end
    
  end
end
