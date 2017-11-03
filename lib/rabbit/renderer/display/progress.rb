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
          @progress_end_id = nil
        end

        def start_progress(max)
          return if max.zero?

          if @progress_end_id
            GLib::Source.remove(@progress_end_id)
            @progress_end_id = nil
            @progress.hide
          end
          update_menu
          @progress.start_progress(max, @canvas.window)
          adjust_progress_window
        end

        def update_progress(i)
          return if @progress_end_id
          @progress.update_progress(i)
          Utils.process_pending_events
        end

        def end_progress
          @progress.end_progress
          @progress_end_id = GLib::Timeout.add(100) do
            @progress.hide
            update_menu
            @progress_end_id = nil
            GLib::Source::REMOVE
          end
        end

        def configured(x, y, w, h)
          super
          adjust_progress_window
        end

        def adjust_progress_window
          if @window and @progress.window
            Utils.move_to_top_left(@window, @progress.window)
          end
        end
      end
    end
  end
end
