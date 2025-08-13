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

require_relative "../../cursor-manager"

module Rabbit
  module Renderer
    module Display
      module Cursor
        def initialize(*args, &block)
          super
          init_cursor
        end

        private
        def init_cursor
          @cursor_manager = CursorManager.new
        end

        def keep_cursor(name)
          @cursor_manager.keep(name)
        end

        def restore_cursor(name)
          @cursor_manager.restore(@surface || @window, name)
        end

        def update_cursor(cursor_type, update_current_cursor=false)
          @cursor_manager.current = cursor_type if update_current_cursor
          cursor_type = :pencil if @graffiti_mode
          @cursor_manager.update(@surface || @window, cursor_type)
        end
      end
    end
  end
end
