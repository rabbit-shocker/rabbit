require "rabbit/cursor-manager"

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
          @cursor_manager.restore(@drawable, name)
        end

        def update_cursor(cursor_type, update_current_cursor=false)
          @cursor_manager.current = cursor_type if update_current_cursor
          cursor_type = :pencil if @graffiti_mode
          @cursor_manager.update(@drawable, cursor_type)
        end
      end
    end
  end
end

