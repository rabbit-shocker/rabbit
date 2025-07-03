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

require "forwardable"

require_relative "../../progress"

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
