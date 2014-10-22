require "rabbit/rabbit"

require "rabbit/renderer/base"

module Rabbit
  module Renderer
    module Print
      module Base
        include Renderer::Base

        attr_writer :filename
        attr_accessor :show_page

        def initialize(canvas)
          super
          @filename = nil
        end

        def page_width
          @page_width - page_margin_left - page_margin_right
        end

        def page_height
          @page_height - page_margin_top - page_margin_bottom
        end

        def width
          page_width
        end

        def height
          page_height
        end

        def paper_width=(value)
          super
          init_paper
        end

        def paper_height=(value)
          super
          init_paper
        end

        def pre_print(slide_size)
          @show_page = true
        end

        def printable?
          true
        end

        def filename
          @filename ||= default_filename
        end

        def draw_slide(slide, simulation)
          internal_draw_slide(slide, simulation) do
            if simulation
              yield
            else
              save_context do
                internal_clip_slide
                internal_draw_background
                yield
              end
            end
          end
        end

        private
        def default_filename
          "#{GLib.filename_from_utf8(@canvas.title.gsub(/\n/, ''))}.pdf"
        end
      end
    end
  end
end
