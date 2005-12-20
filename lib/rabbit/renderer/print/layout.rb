require "forwardable"

require "rabbit/rabbit"

module Rabbit
  module Renderer
    module Print
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
          @margin_left = renderer.margin_left
          @margin_right = renderer.margin_right
          @margin_top = renderer.margin_top
          @margin_bottom = renderer.margin_bottom
        end
      end
      
      class Layout2 < LayoutBase

        def initialize(renderer)
          super
          @margin_left ||= 50
          @margin_right ||= 50
          @margin_top ||= 30
          @margin_bottom ||= 30
        end
        
        def slide_width
          page_width - @margin_left - @margin_right
        end

        def slide_height
          (page_height / 2) - @margin_top - @margin_bottom
        end
        
        def normalize_x(x)
          x + @margin_left
        end

        def normalize_y(y)
          base = @margin_bottom
          if below?
            base += @margin_top + @margin_bottom + slide_height
          end
          y + base
        end

        private
        def below?
          (current_index % 2) == 1
        end
      end
      
      class LayoutMore < LayoutBase
        def initialize(renderer)
          super
          @margin_left ||= 10
          @margin_right ||= 10
          @margin_top ||= 10
          @margin_bottom ||= 10
        end
        
        def slide_width
          (page_width / 2) - @margin_left - @margin_right
        end

        def slide_height
          base = page_height / (slides_per_page / 2.0).ceil
          base - @margin_top - @margin_bottom
        end
        
        def normalize_x(x)
          base = @margin_left
          unless left?
            base += @margin_left + @margin_right + slide_width
          end
          x + base
        end

        def normalize_y(y)
          nth_col = (normalized_current_index / 2.0).truncate
          base = @margin_top * (nth_col + 1)
          base += @margin_bottom * nth_col
          base += slide_height * nth_col
          y + base
        end

        private
        def left?
          (normalized_current_index % 2).zero?
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
