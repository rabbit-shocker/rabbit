require 'rabbit/gtk'

require "rabbit/utils"
require "rabbit/renderer/engine"
require "rabbit/renderer/display/base"
require 'rabbit/renderer/display/cursor'
require 'rabbit/renderer/display/gl'

module Rabbit
  module Renderer
    module Display
      module DrawingAreaPrimitive
        include Base

        include Cursor
        include GL

        attr_accessor :filename
        def initialize(canvas)
          super
          @filename = nil
          clear_compiled_slides
          init_drawing_area
        end

        def attach_to(window, container=nil)
          init_gl(@area)
          super
        end

        def detach
          finalize_gl
          super
        end

        def widget
          @area
        end

        def clear_slide
          super
          clear_compiled_slide
          redraw
        end

        def post_fullscreen
          update_cursor(:blank, true)
          clear_compiled_slides
        end

        def post_unfullscreen
          update_cursor(nil, true)
          update_menu
        end

        def post_iconify
        end

        def post_apply_theme
          clear_compiled_slides
          queue_draw
        end

        def post_move(old_index, index)
          queue_draw
        end

        def post_move_in_slide(old_index, index)
          queue_draw
        end

        def pre_parse
        end

        def post_parse
          clear_compiled_slides
        end

        def pre_toggle_index_mode
        end

        def post_toggle_index_mode
          queue_draw
        end

        def pre_to_pixbuf(slide_size)
        end

        def to_pixbufing(i)
          true
        end

        def post_to_pixbuf(canceled)
        end

        def index_mode_on
          keep_cursor(:index)
          update_cursor(nil, true)
        end

        def index_mode_off
          restore_cursor(:index)
        end

        def display?
          true
        end

        private
        def init_dpi
          @x_dpi = ScreenInfo.screen_x_resolution
          @y_dpi = ScreenInfo.screen_y_resolution
        end

        def init_drawing_area
          @area = Gtk::DrawingArea.new
          @area.can_focus = true
          set_map
          set_draw
          set_configure_event_after
        end

        def depth
          @area.window.depth
        end

        def set_map
          @area.signal_connect_after("map") do |widget|
            mapped(widget)
          end
        end

        def mapped(widget)
          set_drawable(widget.window)
          prepare_renderer(@drawable)
        end

        def set_draw
          stop_events = false
          if @area.class.signals.include?("draw")
            @area.signal_connect("draw") do |widget, context|
              init_context(context)
              draw(widget)
              finish_renderer
              stop_events
            end
          else
            @area.signal_connect("expose_event") do |widget, event|
              init_renderer(@drawable)
              draw(widget)
              finish_renderer
              stop_events
            end
          end
        end

        def draw(widget)
          draw_current_slide
        end

        def draw_current_slide
          slide = @canvas.current_slide
          if slide
            begin
              compile_slide(slide) unless compiled_slide?(slide)
              slide.draw(@canvas, false)
            rescue
              @canvas.logger.warn($!)
            end
          end
        end

        def set_configure_event_after
          prev_x = prev_y = prev_width = prev_height = nil
          @area.signal_connect_after("configure_event") do |widget, event|
            prev_x ||= event.x
            prev_y ||= event.y
            prev_width ||= event.width
            prev_height ||= event.height
            if [prev_x, prev_y, prev_width, prev_height] !=
                [event.x, event.y, event.width, event.height]
              configured_after(widget, event)
            end
            prev_x = event.x
            prev_y = event.y
            prev_width = event.width
            prev_height = event.height
            false
          end
        end

        def configured_after(widget, event)
          reload_theme if @drawable
        end

        def reload_theme(&callback)
          callback ||= Utils.process_pending_events_proc
          @canvas.activate("ReloadTheme", &callback)
        end

        def clear_compiled_slide(slide=nil)
          @compiled_slides.delete(slide || @canvas.current_slide)
        end

        def clear_compiled_slides
          @compiled_slides = {}
        end

        def compiled_slide?(slide)
          @compiled_slides.has_key?(slide)
        end

        def compile_slide(slide)
          @compiled_slides[slide] = true
          slide.draw(@canvas, true)
        end

        def grab
          @area.grab_add
          Gdk.pointer_grab(@area.window, false,
                           Gdk::EventMask::BUTTON_PRESS_MASK |
                           Gdk::EventMask::BUTTON_RELEASE_MASK |
                           Gdk::EventMask::SCROLL_MASK |
                           Gdk::EventMask::POINTER_MOTION_MASK,
                           nil, nil,
                           Gdk::Event::CURRENT_TIME)
        end

        def ungrab
          @area.grab_remove
          Gdk.pointer_ungrab(Gdk::Event::CURRENT_TIME)
        end

        def pointer
          window, x, y, mask = @area.window.pointer
          [x, y, mask]
        end
      end
    end
  end
end
