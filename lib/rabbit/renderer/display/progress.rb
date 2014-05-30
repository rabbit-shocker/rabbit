require "forwardable"

require "rabbit/action"
require "rabbit/progress"

module Rabbit
  module Renderer
    module Display
      module Progress
        extend Forwardable

        def_delegator(:@progress, :foreground, :progress_foreground)
        def_delegator(:@progress, :foreground=, :progress_foreground=)
        def_delegator(:@progress, :background, :progress_background)
        def_delegator(:@progress, :background=, :progress_background=)
        def_delegator(:@progress, :clear_color, :clear_progress_color)

        def initialize(*args, &block)
          super
          init_progress
        end

        private
        def init_progress
          @progress = Rabbit::Progress.new
        end

        def start_progress(max)
          return if max.zero?
          update_menu
          @progress.start_progress(max, @canvas.window)
          adjust_progress_window
        end

        def update_progress(i)
          @progress.update_progress(i)
          Utils.process_pending_events
        end

        def end_progress
          @progress.end_progress
          GLib::Timeout.add(100) do
            @progress.window.hide
            update_menu
            false
          end
        end

        def configured(x, y, w, h)
          super
          adjust_progress_window
        end

        def adjust_progress_window
          if @window
            Utils.move_to_top_left(@window, @progress.window)
          end
        end
      end
    end
  end
end
