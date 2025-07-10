# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require_relative "../../menu"

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

        def toggle_menu(x=nil, y=nil)
          @menu.toggle(x, y)
        end

        def popup_menu(x=nil, y=nil)
          @menu.popup(x, y)
        end

        def popdown_menu
          @menu.popdown
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
