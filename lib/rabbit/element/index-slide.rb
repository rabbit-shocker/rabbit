require 'rabbit/canvas'
require 'rabbit/renderer/pixmap'
require 'rabbit/element/slide-element'

module Rabbit
  module Element
    class IndexSlide
      class ThumbnailPixbuf
        attr_reader :pixbuf, :number
        def initialize(pixbuf, number)
          @pixbuf = pixbuf
          @number = number
        end
      end

      include SlideElement

      ROW_NUMBER = 4
      COLUMN_NUMBER = 4

      class << self
        def make_thumbnails(canvas, width, height)
          maker = make_thumbnail_maker(canvas, width, height)
          maker.apply_theme(canvas.theme_name) if canvas.theme_name

          canvas.source_force_modified(true) do |source|
            maker.parse_rd(source)
          end

          thumbnails = []
          canvas.renderer.pre_to_pixbuf(maker.slide_size)
          canceled = false
          maker.each_slide_pixbuf do |pixbuf, slide_number|
            if canvas.renderer.to_pixbufing(slide_number)
              thumbnails << ThumbnailPixbuf.new(pixbuf, slide_number)
              number_of_slides = slide_number
            else
              canceled = true
            end
            !canceled
          end
          canvas.renderer.post_to_pixbuf(canceled)
          maker.quit

          if canceled
            nil
          else
            thumbnails
          end
        end

        def make_index_slides(canvas)
          width = canvas.width / (COLUMN_NUMBER + 1)
          height = canvas.height / (ROW_NUMBER + 1)

          thumbnails = make_thumbnails(canvas, width, height)
          return [] unless thumbnails

          max_per_slide = ROW_NUMBER * COLUMN_NUMBER
          thumbnails_set = []
          number_of_slides = 0
          thumbnails.each_with_index do |thumbnail, slide_number|
            if slide_number.remainder(max_per_slide).zero?
              thumbnails_set << []
            end
            thumbnails_set.last << thumbnail
            number_of_slides = slide_number
          end

          thumbnails_set.collect do |thumbnails|
            new(thumbnails, number_of_slides)
          end
        end

        private
        def make_thumbnail_maker(canvas, width, height)
          new_canvas = Canvas.new(canvas.logger, Renderer::Pixmap)
          new_canvas.width = width
          new_canvas.height = height
          new_canvas.pango_context = canvas.create_pango_context
          def new_canvas.quit
            nil
          end
          new_canvas
        end
      end

      def initialize(thumbnails, number_of_slides)
        super(_("Index"))
        @thumbnails = thumbnails
        @number_of_slides = number_of_slides
      end

      def width
        sample_pixbuf_property(:width, 0)
      end

      def height
        sample_pixbuf_property(:height, 0)
      end

      def draw(canvas, simulation=nil)
        canvas.draw_slide(self, true) {}
        canvas.draw_slide(self, false) do
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
            text = "#{thumbnail.number}/#{@number_of_slides}"
            text = %Q[<span size="#{text_size}">#{text}</span>]
            layout, _, _ = canvas.make_layout(text)
            layout.set_width(text_width)
            layout.set_alignment(Pango::Layout::ALIGN_CENTER)
            canvas.draw_layout(layout, x, y + h)
          end
        end
      end

      def slide_number(canvas, x, y)
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
