# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../../search-window"

module Rabbit
  module Renderer
    module Display
      module Search
        def initialize(*args, &block)
          super
          init_search
        end

        def search_slide(forward=true)
          if @search_window
            if @search_window.forward? == forward
              search_slide_with_current_input(true)
            else
              @search_window.forward = forward
            end
          else
            setup_search_window(forward)
            adjust_search_window
            @search_window.show
          end
        end

        def stop_slide_search
          @search_window.destroy
          @search_window = nil
        end

        def searching?
          !@search_window.nil?
        end

        private
        def init_search
          @search_window = nil
        end

        def configured(x, y, w, h)
          super
          adjust_search_window
        end

        def adjust_search_window
          if @window and @search_window
            Utils.move_to_bottom_right(@window, @search_window.window)
          end
        end

        def setup_search_window(forward)
          @search_window = SearchWindow.new(@canvas)
          @search_window.forward = forward
          @search_window.window.set_transient_for(@window)
          entry = @search_window.entry
          direction = @search_window.direction
          entry.signal_connect("key_press_event") do |widget, key|
            if key.state == Gdk::ModifierType.new
              if Keys::STOP_SLIDE_SEARCH_KEYS.include?(key.keyval)
                @canvas.activate("StopSlideSearch")
                true
              else
                false
              end
            else
              Gtk::AccelGroup.activate(@window, key.keyval, key.state)
            end
          end
          entry.signal_connect("changed") do
            search_slide_with_current_input
          end
          direction.signal_connect("toggled") do
            search_slide_with_current_input(true)
          end
          entry.signal_connect("activate") do
            search_slide_with_current_input(true)
            true
          end
        end

        def search_slide_with_current_input(search_next=false)
          return if @search_window.empty?
          move_to_matched_slide(@search_window.regexp,
                                @search_window.forward?,
                                search_next)
        end

        def move_to_matched_slide(reg, forward, search_next=false)
          current_index = @canvas.current_index
          indexes = @canvas.slide_indexes(reg)
          target_index = nil
          indexes.each_with_index do |index, i|
            if index == current_index
              target_index = i
              target_index += (forward ? 1 : -1) if search_next
              break
            elsif index > current_index
              target_index = i + (forward ? 0 : -1)
              break
            end
          end
          target_index = indexes.size - 1 if target_index.nil? and !forward
          if target_index and target_index >= 0 and
              target_index < indexes.size and
              indexes[target_index] != current_index
            @canvas.activate("JumpToSlide",
                             GLib::Variant.new(indexes[target_index], "i"))
          end
        end
      end
    end
  end
end
