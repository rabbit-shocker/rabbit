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
          init_paper
          init_color
        end

        def page_width
          @page_width - margin_page_left - margin_page_right
        end
        
        def page_height
          @page_height - margin_page_top - margin_page_bottom
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
          @filename ||= "#{GLib.filename_from_utf8(@canvas.title)}.ps"
        end
        
        private
        def init_color
        end
      end
    end
  end
end

