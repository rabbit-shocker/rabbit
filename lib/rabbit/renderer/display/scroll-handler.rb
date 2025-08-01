# Copyright (C) 2010-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../../gtk"

module Rabbit
  module Renderer
    module Display
      module ScrollHandler
        def initialize(*args, &block)
          super
          init_scroll_handler
        end

        private
        def init_scroll_handler
        end

        if Gtk::Widget.method_defined?(:add_controller)
          ScrollEvent = Struct.new(:direction)

          def set_scroll_event(widget)
            scroll = Gtk::EventControllerScroll.new([:both_axes])
            scroll.signal_connect(:scroll) do |_, dx, dy|
              if dy.zero?
                false
              else
                if dy.positive?
                  direction = Gdk::ScrollDirection::UP
                else
                  direction = Gdk::ScrollDirection::DOWN
                end
                event = ScrollEvent.new(direction)
                handled = call_hook_procs(@scroll_hook_procs, event)
                unless handled
                  if direction == Gdk::ScrollDirection::UP
                    @canvas.activate("NextSlide")
                  else
                    @canvas.activate("PreviousSlide")
                  end
                end
                true
              end
            end
            widget.add_controller(scroll)
          end
        else
          def set_scroll_event(widget)
            widget.add_events(Gdk::EventMask::SCROLL_MASK)
            widget.signal_connect("scroll_event") do |widget, event|
              handled = call_hook_procs(@scroll_hook_procs, event)
              unless handled
                handled = true
                case event.direction
                when Gdk::ScrollDirection::UP
                  @canvas.activate("PreviousSlide")
                when Gdk::ScrollDirection::DOWN
                  @canvas.activate("NextSlide")
                else
                  handled = false
                end
              end
              handled
            end
          end
        end
      end
    end
  end
end
