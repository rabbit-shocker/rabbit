require 'gtk3'

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
            @window.shape_combine_mask(nil, 0, 0)
          else
            setup_mask if @mask.nil?
            w, h = width, height
            @mask.draw_rectangle(@set_gc, true, 0, 0, w, h)
            mw = w * @mask_size
            mh = h * @mask_size
            mx = (w - mw) / 2
            my = (h - mh) / 2
            @mask.draw_rectangle(@xor_gc, true, mx, my, mw, mh)
            @window.shape_combine_mask(@mask, 0, 0)
          end
          update_title # for xfwm
        end

        private
        def init_mask
          @mask = nil
          @mask_size = 0
        end

        def setup_mask
          @mask = Gdk::Pixmap.new(nil, width, height, 1)
          @xor_gc = Gdk::GC.new(@mask)
          @xor_gc.set_function(Gdk::GC::INVERT)
          @set_gc = Gdk::GC.new(@mask)
          @set_gc.set_function(Gdk::GC::SET)
          @clear_gc = Gdk::GC.new(@mask)
          @clear_gc.set_function(Gdk::GC::CLEAR)
        end
      end
    end
  end
end

