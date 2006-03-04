require 'rabbit/canvas'
require 'rabbit/renderer/pixmap'
require 'rabbit/element/container-element'
require 'rabbit/element/slide-element'

module Rabbit
  module Element
    class IndexSlide
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
          number_of_slide = maker.slide_size - 1
          canvas.renderer.pre_to_pixbuf(number_of_slide)
          canceled = false
          maker.each_slide_pixbuf do |pixbuf, slide_number|
            if canvas.renderer.to_pixbufing(slide_number)
              thumbnails << IndexThumbnail.new(pixbuf,
                                               slide_number,
                                               number_of_slide)
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
          width = canvas.width.to_f / (COLUMN_NUMBER + 1)
          height = canvas.height.to_f / (ROW_NUMBER + 1)
          default_margin_top = (height / (ROW_NUMBER + 1) / 2).ceil
          default_margin_bottom = default_margin_top
          default_margin_left = (width / (COLUMN_NUMBER + 1) / 2).ceil
          default_margin_right = default_margin_left

          thumbnails = make_thumbnails(canvas, width, height)
          return [] unless thumbnails

          max_per_slide = ROW_NUMBER * COLUMN_NUMBER
          thumbnail_rows_set = []
          thumbnails.each_with_index do |thumbnail, slide_number|
            if slide_number.remainder(max_per_slide).zero?
              thumbnail_rows_set << []
            end
            if slide_number.remainder(ROW_NUMBER).zero?
              row = IndexThumbnailRow.new
              row.default_margin_top = default_margin_top
              row.default_margin_bottom = default_margin_bottom
              row.default_margin_left = default_margin_left
              row.default_margin_right = default_margin_right
              row.clear_theme
              thumbnail_rows_set.last << row
            end
            thumbnail_rows_set.last.last << thumbnail
          end

          thumbnail_rows_set.collect do |rows|
            index_slide = new(rows)
            index_slide.default_margin_top = default_margin_top
            index_slide.default_margin_bottom = default_margin_bottom
            index_slide.default_margin_left = default_margin_left
            index_slide.default_margin_right = default_margin_right
            index_slide.clear_theme
            index_slide
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

      def initialize(rows)
        super(_("Index"))
        rows.each do |row|
          add_element(row)
        end
      end

      def slide_number(canvas, x, y)
        column = (COLUMN_NUMBER * (x / canvas.width)).to_i
        row = (ROW_NUMBER * (y / canvas.height)).to_i
        thumb = self[row] && self[row][column]
        if thumb
          thumb.number
        else
          nil
        end
      end
    end

    class IndexThumbnailRow
      include ContainerElement
    end

    class IndexThumbnail
      include Base

      attr_reader :pixbuf, :number, :number_of_slides
      def initialize(pixbuf, number, number_of_slides)
        @pixbuf = pixbuf
        @number = number
        @number_of_slides = number_of_slides
        @layout = nil
        super()
      end

      def clear_theme
        super
        @width = @pixbuf.width
        @height = @pixbuf.height
      end

      def draw_element(canvas, x, y, w, h, simulation)
        width = @pixbuf.width
        height = @pixbuf.height
        side_margin = parent.margin_left + parent.margin_right
        unless simulation
          if @layout.nil?
            margin_bottom = parent.margin_bottom
            text_size = (margin_bottom * Pango::SCALE).ceil
            text = "#{@number}/#{@number_of_slides}"
            text = %Q[<span size="#{text_size}">#{text}</span>]
            @layout, _, _ = canvas.make_layout(text)
            @layout.set_width(width * Pango::SCALE)
            @layout.set_alignment(Pango::Layout::ALIGN_CENTER)
          end
          canvas.draw_pixbuf(@pixbuf, x, y)
          canvas.draw_rectangle(false, x, y, width, height)
          canvas.draw_layout(@layout, x, y + height)
        end
        [x + width + side_margin, y, w - width - side_margin, h]
      end

      def to_html(generator)
        number_of_places = generator.number_of_places(@number_of_slides)
        format = "thumbnail%0#{number_of_places}d"
        src = generator.save_pixbuf(@pixbuf, format % @number)
        title = generator.image_title
        "<img title=\"#{title}\" src=\"#{src}\" />"
      end
    end
  end
end
