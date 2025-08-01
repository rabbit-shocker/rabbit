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

require_relative "drawing-area-primitive"
require_relative "menu"
require_relative "progress"
require_relative "mask"
require_relative "search"
require_relative "gesture"
require_relative "graffiti"
require_relative "button-handler"
require_relative "scroll-handler"
require_relative "info"
require_relative "spotlight"
require_relative "magnifier"

module Rabbit
  module Renderer
    module Display
      module DrawingAreaBase
        include DrawingAreaPrimitive

        include Menu
        include Graffiti
        include Mask
        include Progress
        include Search
        include Gesture
        include ButtonHandler
        include ScrollHandler
        include Info
        include Spotlight
        include Magnifier

        def initialize(canvas)
          @caching = nil
          @need_reload_theme = false
          super
        end

        def update_size(width, height)
          super
          @mask = nil
          set_hole
          reload_theme if @surface
        end

        def post_apply_theme
          if @need_reload_theme
            @need_reload_theme = false
            reload_theme
          else
            super
            update_menu
          end
        end

        def post_move(old_index, index)
          update_title
          reset_adjustment
          clear_graffiti
          # toggle_graffiti_mode if @graffiti_mode
          super
        end

        def post_fullscreen
          super
          update_menu
        end

        def post_unfullscreen
          super
          update_menu
        end

        def post_iconify
          super
          update_menu
        end

        def pre_parse
          super
          update_menu
        end

        def post_parse
          super
          clear_button_handler
          update_title
          update_menu
          if @need_reload_theme
            @need_reload_theme = false
            reload_theme
          end
        end

        def index_mode_on
          super
        end

        def index_mode_off
          super
        end

        def pre_toggle_index_mode
          super
          Utils.process_pending_events
        end

        def post_toggle_index_mode
          @canvas.activate("ClearGraffiti")
          update_menu
          update_title
          super
        end

        def pre_print(n_slides)
          start_progress(n_slides)
        end

        def printing(i)
          update_progress(i)
          continue = !@canvas.quitted?
          continue
        end

        def post_print(canceled)
          end_progress
        end

        def pre_to_pixbuf(n_slides)
          super
          start_progress(n_slides)
          @pixbufing_size = [width, height]
        end

        def to_pixbufing(i)
          update_progress(i)
          continue = @pixbufing_size == [width, height] &&
            !@canvas.quitted? && !@canvas.applying?
          super or continue
        end

        def post_to_pixbuf(canceled)
          super
          end_progress
        end

        def cache_all_slides
          pre_cache_all_slides(@canvas.n_slides)
          canceled = false
          @canvas.slides.each_with_index do |slide, i|
            @canvas.change_current_index(i) do
              compile_slide(slide)
            end
            unless caching_all_slides(i)
              canceled = true
              break
            end
          end
          post_cache_all_slides(canceled)
        end

        def pre_cache_all_slides(n_slides)
          @caching = true
          @caching_size = [width, height]
          start_progress(n_slides)
        end

        def caching_all_slides(i)
          update_progress(i)
          continue = @caching_size == [width, height] &&
            !@canvas.quitted? && !@canvas.applying?
          continue
        end

        def post_cache_all_slides(canceled)
          end_progress
          @caching = false
          return if @canvas.quitted?
          if canceled
            reload_theme
          else
            @area.queue_draw
          end
        end

        def confirm(message)
          confirm_dialog(message) == Gtk::ResponseType::OK
        end

        def reload_theme(&callback)
          if @canvas.applying?
            @need_reload_theme = true
          else
            super
          end
        end

        def reload_source(&callback)
          if @canvas.need_reload_source?
            callback ||= Utils.process_pending_events_proc
            begin
              super(callback)
            rescue
              Rabbit.logger.error($!)
            end
          end
        end

        def attach_to(window, container=nil, &block)
          super

          init_menu
          init_gesture_actions
          add_widgets_to_container(@container, &block)
          widget.show
          attach_menu(@window)
        end

        def detach
          detach_menu(@window)
          widget.hide
          unless @window.destroyed?
            remove_widgets_from_container(@container)
          end

          super
        end

        def toggle_whiteout
          super
          @area.queue_draw
        end

        def toggle_blackout
          super
          @area.queue_draw
        end

        def reset_adjustment
          super
          @area.queue_draw
        end

        def post_init_gui
        end

        def draw_slide(slide, simulation, &block)
          super do |*args|
            block.call(*args)
            magnify {block.call(*args)} unless simulation
          end
        end

        private
        def add_widgets_to_container(container)
          @hbox = Gtk::Box.new(:horizontal)
          @vbox = Gtk::Box.new(:vertical)
          @vbox.pack_start(@area, :expand => true, :fill => true, :padding => 0)
          @hbox.pack_end(@vbox, :expand => true, :fill => true, :padding => 0)
          if block_given?
            yield(container, @hbox)
          else
            container.add(@hbox)
          end
          @hbox.show
          @vbox.show
        end

        def remove_widgets_from_container(container)
          container.remove(@hbox)
          @hbox = @vbox = nil
        end

        def init_drawing_area
          super
          set_button_event(@area)
          @area.add_events(Gdk::EventMask::BUTTON1_MOTION_MASK |
                           Gdk::EventMask::BUTTON2_MOTION_MASK |
                           Gdk::EventMask::BUTTON3_MOTION_MASK)
          set_motion_notify_event
          set_scroll_event(@area)
        end

        def set_motion_notify_event
          @area.signal_connect("motion_notify_event") do |widget, event|
            call_hook_procs(@motion_notify_hook_procs, event)
          end
        end

        def paint(color_name)
          context = @surface.create_cairo_context
          context.set_source_rgba(*Color.parse(color_name).to_a)
          context.paint
        end

        def draw(widget)
          reload_source unless @caching

          if whiteouting?
            paint("white")
          elsif blackouting?
            paint("black")
          else
            super
            draw_graffiti
            draw_gesture
            draw_spotlight
          end
        end

        def draw_current_slide_pixbuf(pixbuf)
          width, height = pixbuf.width, pixbuf.height
          x = @adjustment_x * width
          y = @adjustment_y * height
          @surface.draw_pixbuf(@foreground, pixbuf,
                               x, y, 0, 0, width, height,
                               Gdk::RGB::DITHER_NORMAL, 0, 0)
          if @adjustment_x != 0 or @adjustment_y != 0
            draw_next_slide
          end
        end

        def draw_next_slide
          @canvas.change_current_index(@canvas.current_index + 1) do
            draw_current_slide do |pixbuf|
              draw_next_slide_pixbuf(pixbuf)
            end
          end
        end

        def draw_next_slide_pixbuf(pixbuf)
          width, height = pixbuf.size
          adjustment_width = @adjustment_x * width
          adjustment_height = @adjustment_y * height
          src_x = src_y = dest_x = dest_y = 0
          src_width = width
          src_height = height

          if adjustment_width > 0
            dest_x = width - adjustment_width
            src_width = adjustment_width
          elsif adjustment_width < 0
            src_x = width + adjustment_width
            src_width = -adjustment_width
          end

          if adjustment_height > 0
            dest_y = height - adjustment_height
            src_height = adjustment_height
          elsif adjustment_height < 0
            src_y = height + adjustment_height
            src_height = -adjustment_height
          end

          @surface.draw_pixbuf(@foreground, pixbuf, src_x, src_y,
                               dest_x, dest_y, src_width, src_height,
                               Gdk::RGB::DITHER_NORMAL, 0, 0)
        end

        def confirm_dialog(message)
          flags = Gtk::Dialog::MODAL | Gtk::Dialog::DESTROY_WITH_PARENT
          dialog_type = Gtk::MessageDialog::INFO
          buttons = Gtk::MessageDialog::BUTTONS_OK_CANCEL
          dialog = Gtk::MessageDialog.new(nil, flags, dialog_type,
                                          buttons, message)
          result = dialog.run
          dialog.destroy
          result
        end
      end
    end
  end
end
