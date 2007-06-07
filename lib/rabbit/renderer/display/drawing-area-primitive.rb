require 'gtk2'

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

        def attach_to(window)
          init_gl(@area)
          @area.show
          @window = window
        end

        def detach
          finalize_gl
          @area.hide
          @window = nil
        end

        def width
          if @drawable
            @drawable.size[0]
          end
        end
        alias original_width width

        def height
          if @drawable
            @drawable.size[1]
          end
        end
        alias original_height height

        def redraw
          @area.queue_draw
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
          update_menu
        end

        def post_iconify
        end

        def post_apply_theme
          clear_compiled_slides
          @area.queue_draw
        end

        def post_move(index)
          @area.queue_draw
        end

        def pre_parse
        end

        def post_parse
          clear_compiled_slides
        end

        def pre_toggle_index_mode
        end

        def post_toggle_index_mode
          @area.queue_draw
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

        def make_layout(text)
          attrs, text = Pango.parse_markup(text)
          layout = create_pango_layout(text)
          layout.set_attributes(attrs)
          layout
        end

        def create_pango_context
          @area.create_pango_context
        end

        def create_pango_layout(text)
          @area.create_pango_layout(text)
        end

        def display?
          true
        end

        def draw_slide(slide, simulation)
          init_renderer(@drawable) unless simulation
          super
        end

        private
        def init_dpi
          @x_dpi = ScreenInfo.screen_x_resolution
          @y_dpi = ScreenInfo.screen_y_resolution
        end

        def init_drawing_area
          @area = Gtk::DrawingArea.new
          @area.can_focus = true
          set_realize
          set_expose_event
          set_configure_event_after
        end

        def set_realize
          @area.signal_connect_after("realize") do |widget|
            realized(widget)
          end
        end

        def realized(widget)
          @drawable = widget.window
          init_renderer(@drawable)
        end

        def set_expose_event
          @area.signal_connect("expose_event") do |widget, event|
            exposed(widget, event)
          end
        end

        def exposed(widget, event)
          draw_current_slide
          true
        end

        def draw_current_slide
          slide = @canvas.current_slide
          if slide
            compile_slide(slide) unless compiled_slide?(slide)
            slide.draw(@canvas, false)
          end
        end

        def set_configure_event_after
          @area.signal_connect_after("configure_event") do |widget, event|
            configured_after(widget, event)
          end
        end

        def configured_after(widget, event)
          reload_theme if @drawable
          false
        end

        def reload_theme(&callback)
          callback ||= Utils.process_pending_events_proc
          @canvas.activate("ReloadTheme", &callback)
          clear_compiled_slides
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

        def queue_draw
          @area.queue_draw
        end
      end
    end
  end
end
