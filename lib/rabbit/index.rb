require 'gtk2'

require 'rabbit/frame'
require 'rabbit/renderer/drawing-area'

module Rabbit

  module Index

    ROW_NUMBER = 4
    COLUMN_NUMBER = 4
    
    class << self
      def make_index_pages(canvas)
        thumbnail_width = canvas.width / (COLUMN_NUMBER + 1)
        thumbnail_height = canvas.height / (ROW_NUMBER + 1)

        maker = make_thumbnail_maker(canvas, thumbnail_width, thumbnail_height)
        maker.apply_theme(canvas.theme_name) if canvas.theme_name

        canvas.source_force_modified(true) do |source|
          maker.parse_rd(source)
        end

        max_per_page = ROW_NUMBER * COLUMN_NUMBER
        thumbnails_set = []
        number_of_pages = 0
        canvas.renderer.pre_to_pixbuf(maker.page_size)
        maker.each_page_pixbuf do |pixbuf, page_number|
          canvas.renderer.to_pixbufing(page_number)
          if page_number.remainder(max_per_page).zero?
            thumbnails_set << []
          end
          thumbnails_set.last << ThumbnailPixbuf.new(pixbuf, page_number)
          number_of_pages = page_number
        end
        canvas.renderer.post_to_pixbuf
        maker.quit
        
        thumbnails_set.collect do |thumbnails|
          Page.new(thumbnails, number_of_pages)
        end
      end

      private
      def make_thumbnail_maker(canvas, width, height)
        make_thumbnail_maker_without_frame(canvas, width, height)
      end
      
      def make_thumbnail_maker_without_frame(canvas, width, height)
        new_canvas = Canvas.new(canvas.logger, Renderer::Pixmap)
        new_canvas.width = width
        new_canvas.height = height
        new_canvas.pango_context = canvas.create_pango_context
        def new_canvas.quit
          nil
        end
        new_canvas
      end

      def make_thumbnail_maker_with_frame(canvas, width, height)
        new_canvas = Canvas.new(canvas.logger, Renderer::DrawingArea)
        frame = Frame.new(canvas.logger, new_canvas)
        frame.init_gui(width, height, false)
        frame
      end
    end

    class ThumbnailPixbuf
      attr_reader :pixbuf, :number
      def initialize(pixbuf, number)
        @pixbuf = pixbuf
        @number = number
      end
    end
    
    
    class Page

      def initialize(thumbnails, number_of_pages)
        @thumbnails = thumbnails
        @number_of_pages = number_of_pages
      end

      def title
        "Index"
      end
      
      def width
        sample_pixbuf_property(:width, 0)
      end
      
      def height
        sample_pixbuf_property(:height, 0)
      end
      
      def draw(canvas)
        canvas.draw_page(self) do
          w = width
          h = height
          x_base = (w / (COLUMN_NUMBER + 1.0)).ceil
          x_step = w + x_base
          y_base = (h / (ROW_NUMBER + 1.0)).ceil
          y_step = h + y_base
          x = 0
          y = -h
          text_width = w * Pango::SCALE
          text_size = (y_base * 0.5 * Pango::SCALE).ceil
          params = {'width' => w, 'height' => h}
          @thumbnails.each_with_index do |thumbnail, i|
            if i.remainder(COLUMN_NUMBER).zero?
              x = x_base
            else
              x += x_step
            end
            if i.remainder(ROW_NUMBER).zero?
              y += y_step
            end
            canvas.draw_pixbuf(thumbnail.pixbuf, x, y, params)
            canvas.draw_rectangle(false, x, y, w, h)
            text = "#{thumbnail.number}/#{@number_of_pages}"
            text = %Q[<span size="#{text_size}">#{text}</span>]
            layout, _, _ = canvas.make_layout(text)
            layout.set_width(text_width)
            layout.set_alignment(Pango::Layout::ALIGN_CENTER)
            canvas.draw_layout(layout, x, y + h)
          end
        end
      end

      def page_number(canvas, x, y)
        column = (COLUMN_NUMBER * (x / canvas.width)).to_i
        row = (ROW_NUMBER * (y / canvas.height)).to_i
        thumb = @thumbnails[row * ROW_NUMBER + column]
        if thumb.nil?
          nil
        else
          thumb.number
        end
      end
      
      private
      def sample_pixbuf
        if @thumbnails.empty?
          nil
        else
          @thumbnails.first.pixbuf
        end
      end

      def sample_pixbuf_property(name, default)
        pixbuf = sample_pixbuf
        if pixbuf.respond_to?(name)
          pixbuf.__send__(name)
        else
          default
        end
      end
      
    end
    
  end
end
