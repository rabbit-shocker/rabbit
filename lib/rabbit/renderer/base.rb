require "gtk2"

require "rabbit/rabbit"
require "rabbit/gettext"

module Rabbit
  module Renderer

    module Base
      include GetText

      def initialize(canvas)
        @canvas = canvas
        @font_families
      end
      
      def font_families
        if @font_families.nil? or @font_families.empty?
          layout = Pango::Layout.new(create_pango_context)
          @font_families = layout.context.list_families
        end
        @font_families
      end

      def print(&block)
        if printable?
          do_print(&block)
        else
          canvas = make_canvas_with_printable_renderer
          pre_print(canvas.page_size)
          canvas.print do |i|
            printing(i)
          end
          post_print
        end
      end

      def each_page_pixbuf
        if can_create_pixbuf?
          canvas = @canvas
        else
          canvas = make_canvas_with_offscreen_renderer
        end
        previous_index = canvas.current_index
        pre_to_pixbuf(canvas.page_size)
        canvas.pages.each_with_index do |page, i|
          to_pixbufing(i)
          canvas.move_to_if_can(i)
          page.draw(canvas)
          yield(canvas.to_pixbuf(page), i)
        end
        post_to_pixbuf
        canvas.move_to_if_can(previous_index)
      end
      
      def create_pango_context
        Pango::Context.new
      end
      
      private
      def printable?
        false
      end

      def can_create_pixbuf?
        false
      end
      
      def do_print(&block)
        pre_print(@canvas.page_size)
        @canvas.pages.each_with_index do |page, i|
          @canvas.move_to_if_can(i)
          @canvas.current_page.draw(@canvas)
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
        make_canvas_with_renderer(GnomePrint) do |canvas|
        end
      end
      
      def make_canvas_with_offscreen_renderer
        make_canvas_with_renderer(Pixmap) do |canvas|
          canvas.width = @canvas.width
          canvas.height = @canvas.height
        end
      end

    end
    
  end
end
