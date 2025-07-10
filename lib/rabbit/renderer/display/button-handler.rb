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

require_relative "../../gtk"

module Rabbit
  module Renderer
    module Display
      module ButtonHandler
        BUTTON_PRESS_ACCEPTING_TIME = 250

        def initialize(*args, &block)
          super
          init_button_handler
        end

        private
        def init_button_handler
          @button_handing = false
          clear_button_handler
        end

        def clear_button_handler
          @button_handler = []
        end

        if Gtk.const_defined?(:GestureClick)
          ButtonPressEvent = Struct.new(:button,
                                        :x,
                                        :y,
                                        :state) do
            def event_type
              Gdk::EventType::BUTTON_PRESSS
            end
          end
          ButtonReleaseEvent = Struct.new(:button,
                                          :x,
                                          :y,
                                          :state) do
            def event_type
              Gdk::EventType::BUTTON_RELEASE
            end
          end

          def set_button_event(widget)
            ["primary", "middle", "secondary"].each do |button_type|
              click = Gtk::GestureClick.new
              click.name = "rabbit-click-#{button_type}"
              click.button = Gdk.const_get("BUTTON_#{button_type.upcase}")

              last_button_press_event = nil
              click.signal_connect(:pressed) do |_, n_presses, x, y|
                last_button_press_event =
                  ButtonPressEvent.new(click.current_button,
                                       x,
                                       y,
                                       click.current_event_state)
                call_hook_procs(@button_press_hook_procs,
                                last_button_press_event)
              end

              click.signal_connect(:released) do |_, n_presses, x, y|
                event = ButtonReleaseEvent.new(click.current_button,
                                               x,
                                               y,
                                               click.current_event_state)
                if last_button_press_event
                  handled = call_hook_procs(@button_release_hook_procs,
                                            event,
                                            last_button_press_event)
                  unless handled
                    click_type = nil
                    case n_presses
                    when 1
                      click_type = "single_click"
                    when 2
                      click_type = "double_click" if button_type == "primary"
                    end
                    if click_type
                      handler = "handle_button_#{button_type}_#{click_type}"
                      __send__(handler, last_button_press_event, event)
                      start_button_handler
                    end
                  end
                else
                  clear_button_handler
                end
              end

              widget.add_controller(click)
            end
          end
        else
          def set_button_event(widget)
            widget.add_events(Gdk::EventMask::BUTTON_PRESS_MASK |
                              Gdk::EventMask::BUTTON_RELEASE_MASK)
            last_button_press_event = nil
            widget.signal_connect("button_press_event") do |_widget, event|
              last_button_press_event = event
              call_hook_procs(@button_press_hook_procs, event)
            end

            widget.signal_connect("button_release_event") do |_widget, event|
              if last_button_press_event
                handled = call_hook_procs(@button_release_hook_procs,
                                          event, last_button_press_event)
                if handled
                  clear_button_handler
                else
                  handled = handle_button_release(event, last_button_press_event)
                end
                handled
              else
                clear_button_handler
                false
              end
            end
          end

          def handle_button_release(event, last_button_press_event)
            button_types = {
              1 => "primary",
              2 => "middle",
              3 => "secondary",
              5 => "primary",
              4 => "middle",
            }
            click_types = {
              Gdk::EventType::BUTTON_PRESS => "single_click",
              Gdk::EventType::BUTTON2_PRESS => "double_click",
            }
            button_type = button_types[event.button]
            click_type = click_types[last_button_press_event.event_type]
            if button_type and click_type
              handler = "handle_button_#{button_type}_#{click_type}"
              __send__(handler, last_button_press_event, event)
              start_button_handler
            end
            true
          end
        end

        def handle_button_primary_single_click(event, release_event)
          if release_event.state.control_mask?
            add_button_handler do
              toggle_menu(release_event.x, release_event.y)
              true
            end
          elsif !release_event.state.alt_mask?
            add_button_handler do
              popdown_menu
              @canvas.activate("NextSlide")
              true
            end
          end
        end

        def handle_button_middle_single_click(event, release_event)
          unless release_event.state.alt_mask?
            add_button_handler do
              popdown_menu
              @canvas.activate("PreviousSlide")
              true
            end
          end
        end

        def handle_button_secondary_single_click(event, release_event)
          add_button_handler do
            toggle_menu(release_event.x, release_event.y)
            true
          end
        end

        def handle_button_primary_double_click(event, release_event)
          add_button_handler do
            if @canvas.index_mode?
              index = @canvas.current_slide.slide_number(@canvas,
                                                         event.x,
                                                         event.y)
              if index
                @canvas.activate("ToggleIndexMode")
                @canvas.activate("JumpToSlide", GLib::Variant.new(index, "i"))
              end
              true
            else
              false
            end
          end
        end

        def add_button_handler(handler=nil, &block)
          handler ||= Proc.new(&block)
          @button_handler.push(handler)
        end

        def call_button_handler
          until @button_handler.empty?
            clear_button_handler if @button_handler.pop.call
          end
        end

        def start_button_handler
          if @button_handling
            @button_event_coming = true
          else
            @button_handling = true
            @button_event_coming = false
            GLib::Timeout.add(BUTTON_PRESS_ACCEPTING_TIME) do
              if @button_event_coming
                GLib::Timeout.add(BUTTON_PRESS_ACCEPTING_TIME) do
                  call_button_handler
                  @button_handling = false
                  false
                end
              else
                call_button_handler
                @button_handling = false
              end
              false
            end
          end
        end
      end
    end
  end
end
