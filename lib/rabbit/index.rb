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

        new_canvas = Canvas.new(canvas.logger, Renderer::DrawingArea)
        frame = Frame.new(canvas.logger, new_canvas)
        init_gui_args = [thumbnail_width, thumbnail_height, false]
        frame.init_gui(*init_gui_args)
        frame.apply_theme(canvas.theme_name) if canvas.theme_name

        source = canvas.source
        mod = source.force_modified
        source.force_modified = true
        frame.parse_rd(source)
        source.force_modified = mod

        max_per_page = ROW_NUMBER * COLUMN_NUMBER
        thumbnails_set = []
        number_of_pages = 0
        frame.each_page_pixbuf do |pixbuf, page_number|
          if page_number.remainder(max_per_page).zero?
            thumbnails_set << []
          end
          thumbnails_set.last << ThumbnailPixbuf.new(pixbuf, page_number)
          number_of_pages = page_number
        end
        frame.quit
        
        thumbnails_set.collect do |thumbnails|
          Page.new(thumbnails, number_of_pages)
        end
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
