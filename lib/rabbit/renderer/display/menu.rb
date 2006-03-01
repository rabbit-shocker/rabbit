require "rabbit/menu"

module Rabbit
  module Renderer
    module Display
      module Menu
        def initialize(*args, &block)
          super
          # init_menu
        end

        private
        def init_menu
          @menu = Rabbit::Menu.new(@canvas.actions)
        end

        def update_menu
          @menu.update_menu(@canvas)
        end

        def detach_menu(window)
          @menu.detach(window)
        end
      end
    end
  end
end
