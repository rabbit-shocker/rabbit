require 'rabbit/info-window'

module Rabbit
  module Renderer
    module Display
      module Info
        def initialize(canvas)
          super
          @info_window = InfoWindow.new(@canvas)
        end

        def toggle_info_window
          if @info_window.showing?
            hide_info_window
          else
            show_info_window
          end
        end

        def show_info_window
          @info_window.show(width, height)
        end

        def hide_info_window
          @info_window.hide
        end

        def post_move(old_index, index)
          super
          @info_window.moved(index)
        end

        def index_mode_on
          super
          @info_window.index_mode_on
        end

        def index_mode_off
          super
          @info_window.index_mode_off
        end
      end
    end
  end
end
