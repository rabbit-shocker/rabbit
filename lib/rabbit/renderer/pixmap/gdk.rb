require "rabbit/renderer/engine/gdk"
require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    module Pixmap
      class GDK
        include Base
        include Engine::GDK
        
        class << self
          def priority
            0
          end
        end

        def initialize(canvas, width=nil, height=nil)
          super
        end
        
        def foreground=(color)
          @foreground.set_rgb_fg_color(color.to_gdk_color)
        end
        
        def background=(color)
          @background.set_rgb_fg_color(color.to_gdk_color)
        end
        
        def background_image=(pixbuf)
          w, h = pixbuf.width, pixbuf.height
          pixmap = Gdk::Pixmap.new(nil, w, h, depth)
          pixmap.draw_rectangle(@background, true, 0, 0, w, h)
          args = [
                  @foreground, pixbuf,
                  0, 0, 0, 0, w, h,
                  Gdk::RGB::DITHER_NORMAL, 0, 0,
                 ]
          pixmap.draw_pixbuf(*args)
          @background.set_tile(pixmap)
          @background.fill = Gdk::GC::Fill::TILED
        end
        
        def draw_slide(slide, simulation)
          init_pixmap(slide, simulation)
          unless simulation
            draw_rectangle(true, 0, 0, width, height, @background)
          end
          yield
        end

        private
        def init_color
          super
          @foreground = Gdk::GC.new(drawable)
          @background = make_gc_from_string(@background_color)
        end
      end
    end
  end
end
