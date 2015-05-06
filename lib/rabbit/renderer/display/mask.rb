require 'rabbit/gtk'

module Rabbit
  module Renderer
    module Display
      module Mask
        MASK_SIZE_STEP = 0.05

        def initialize(*args, &block)
          super
          init_mask
        end

        def expand_hole
          if @mask_size < 0
            @mask_size = MASK_SIZE_STEP
          else
            @mask_size = [@mask_size + MASK_SIZE_STEP, 1.0].min
          end
          set_hole
        end

        def narrow_hole
          if @mask_size < 0
            @mask_size = 0
          else
            @mask_size = [@mask_size - MASK_SIZE_STEP, 0.0].max
          end
          set_hole
        end

        def set_hole
          if @mask_size <= 0
            @window.shape_combine_region(nil)
          else
            _size = size
            w, h = _size.real_width, _size.real_height
            @mask = Cairo::Region.new
            @mask.union!(0, 0, w, h)
            if @mask_size < 1.0
              mw = w * @mask_size
              mh = h * @mask_size
              mx = (w - mw) / 2
              my = (h - mh) / 2
              @mask.subtract!(mx, my, mw, mh)
            else
              @mask.subtract!(0, 0, w - 1, h)
            end
            @window.shape_combine_region(@mask)
          end
          update_title # for xfwm
        end

        private
        def init_mask
          @mask_size = 0
        end
      end
    end
  end
end

