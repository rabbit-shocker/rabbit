require "rabbit/menu"

module Rabbit
  module Renderer
    module Display
      module Menu
        private
        def init_menu
          @menu = Rabbit::Menu.new(@canvas.actions)
        end

        def update_menu
          @menu.update_menu(@canvas)
        end

        def popup_menu
          @menu.popup(0, Gtk.current_event_time)
        end

        def attach_menu(window)
          @menu.attach(window)
        end

        def detach_menu(window)
          @menu.detach(window)
        end
      end
    end
  end
end
