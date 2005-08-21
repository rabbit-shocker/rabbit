require "forwardable"

require "rabbit/renderer/print"

module Rabbit
  module Renderer
    class MultiplePrint < Print
      include Base
      extend Forwardable
      
      def_delegators(:@canvas, :current_index)

      def width
        @slide_width ||= @layout.slide_width
      end

      def height
        @slide_height ||= @layout.slide_height
      end

      def draw_slide(slide, simulation)
        # @context.begin_page(slide.title) do
        @context.begin_page(page_title) if need_begin_page?
        draw_background unless simulation
        yield
        @context.show_page if need_show_page?
      end

      def slides_per_page=(slides)
        super
        update_layout
      end
      
      private
      def init_paper
        pt = unit("Pt")
        paper = Gnome::PrintPaper.get("A4")
        @config[Gnome::PrintConfig::KEY_PAPER_SIZE] = "A4"
        @config.set(Gnome::PrintConfig::KEY_PAPER_WIDTH, paper.width, pt)
        @config.set(Gnome::PrintConfig::KEY_PAPER_HEIGHT, paper.height, pt)
        @page_width = get_length_by_point(Gnome::PrintConfig::KEY_PAPER_WIDTH)
        @page_height = get_length_by_point(Gnome::PrintConfig::KEY_PAPER_HEIGHT)
      end
      
      def update_layout
        if @slides_per_page
          @layout = LayoutBase.make_layout(self)
        else
          @layout = nil
        end
        @slide_width = nil
        @slide_height = nil
      end

      def draw_background
        super
        draw_rectangle(false, 0, 0, width, height, @black.to_s)
      end
      
      def normalize_x(x)
        @layout.normalize_x(x)
      end
      alias nx normalize_x

      def normalize_y(y)
        @layout.normalize_y(y)
      end
      alias ny normalize_y
      
      def need_begin_page?
        (current_index % @slides_per_page).zero?
      end

      def need_show_page?
        @canvas.last_slide? or ((current_index + 1) % @slides_per_page).zero?
      end

      def page_title
        last = [current_index + @slides_per_page - 1, @canvas.slide_size].min
        "#{@canvas.current_index}-#{last}"
      end

      def from_screen(x, y)
        super(nx(x), ny(y))
      end
      
      class LayoutBase
        extend Forwardable

        class << self
          def make_layout(renderer)
            map = LAYOUT_MAPPING.find do |key, value|
              key === renderer.slides_per_page
            end
            if map.nil?
              raise InvalidPages.new(renderer.slides_per_page)
            else
              map[1].new(renderer)
            end
          end
        end

        def_delegators(:@renderer, :slides_per_page)
        def_delegators(:@renderer, :current_index)
        def_delegators(:@renderer, :page_width, :page_height)

        def initialize(renderer)
          @renderer = renderer
          @left_margin = renderer.left_margin
          @right_margin = renderer.right_margin
          @top_margin = renderer.top_margin
          @bottom_margin = renderer.bottom_margin
        end
      end
      
      class Layout2 < LayoutBase

        def initialize(renderer)
          super
          @left_margin ||= 50
          @right_margin ||= 50
          @top_margin ||= 30
          @bottom_margin ||= 30
        end
        
        def slide_width
          page_width - @left_margin - @right_margin
        end

        def slide_height
          (page_height / 2) - @top_margin - @bottom_margin
        end
        
        def normalize_x(x)
          x + @left_margin
        end

        def normalize_y(y)
          base = @bottom_margin
          if above?
            base += @top_margin + @bottom_margin + slide_height
          end
          y - base
        end

        private
        def above?
          (current_index % 2).zero?
        end
      end
      
      class LayoutMore < LayoutBase
        def initialize(renderer)
          super
          @left_margin ||= 10
          @right_margin ||= 10
          @top_margin ||= 10
          @bottom_margin ||= 10
        end
        
        def slide_width
          (page_width / 2) - @left_margin - @right_margin
        end

        def slide_height
          base = page_height / (slides_per_page / 2.0).ceil
          base - @top_margin - @bottom_margin
        end
        
        def normalize_x(x)
          base = @left_margin
          unless left?
            base += @left_margin + @right_margin + slide_width
          end
          x + base
        end

        def normalize_y(y)
          negative_index = normalized_slides - normalized_current_index
          nth_col = (negative_index / 2.0).ceil - 1
          base = @top_margin * nth_col
          base += @bottom_margin * (nth_col + 1)
          base += slide_height * nth_col
          y - base
        end

        private
        def left?
          (normalized_current_index % 2).zero?
        end

        def normalized_slides
          if (slides_per_page % 2).zero?
            slides_per_page
          else
            slides_per_page + 1
          end
        end

        def normalized_current_index
          current_index % slides_per_page
        end
      end

      LAYOUT_MAPPING = [
        [2, Layout2],
        [3..(1.0/0), LayoutMore],
      ]
    end
  end
end
