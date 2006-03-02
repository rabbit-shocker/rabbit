require 'gtk2'

require "rabbit/utils"
require "rabbit/renderer/engine"
require "rabbit/renderer/display/base"
require "rabbit/renderer/display/progress"
require "rabbit/renderer/display/mask"
require "rabbit/renderer/display/cursor"
require "rabbit/renderer/display/search"
require "rabbit/renderer/display/gesture"
require "rabbit/renderer/display/graffiti"
require "rabbit/renderer/display/menu"
require "rabbit/renderer/display/button-handler"
require "rabbit/renderer/display/key-handler"

module Rabbit
  module Renderer
    module Display
      module DrawingAreaBase
        include Base

        include Graffiti
        include Mask
        include Progress
        include Cursor
        include Search
        include Gesture
        include KeyHandler
        include ButtonHandler

        attr_accessor :filename
        def initialize(canvas)
          super
          @filename = nil
          @caching = nil
          @need_reload_theme = false
          clear_compiled_slides
          init_drawing_area
        end

        def attach_to(window)
          init_menu
          init_gesture_actions
          @window = window
          @hbox = Gtk::HBox.new
          @vbox = Gtk::VBox.new
          @vbox.pack_start(@area, true, true, 0)
          @area.show
          @hbox.pack_end(@vbox, true, true, 0)
          @window.add(@hbox)
          @hbox.show
          @vbox.show
          attach_menu
          attach_key
          set_configure_event
        end

        def detach
          detach_key
          detach_menu
          @window.remove(@hbox)
          @hbox = @vbox = @area = nil
          @window.signal_handler_disconnect(@configure_signal_id)
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

        def post_apply_theme
          if @need_reload_theme
            @need_reload_theme = false
            reload_theme
          else
            clear_compiled_slides
            update_menu
            @area.queue_draw
          end
        end

        def post_move(index)
          update_title
          reset_adjustment
          clear_graffiti
          # toggle_graffiti_mode if @graffiti_mode
          @area.queue_draw
        end

        def post_fullscreen
          update_cursor(:blank, true)
          clear_compiled_slides
          update_menu
        end

        def post_unfullscreen
          update_cursor(nil, true)
          clear_compiled_slides
          update_menu
        end

        def post_iconify
          update_menu
        end

        def redraw
          clear_compiled_slide
          @area.queue_draw
        end

        def pre_parse_rd
          update_menu
        end

        def post_parse_rd
          clear_button_handler
          update_title
          update_menu
          clear_compiled_slides
          if @need_reload_theme
            @need_reload_theme = false
            reload_theme
          end
        end

        def index_mode_on
          keep_cursor(:index)
          update_cursor(nil, true)
        end

        def index_mode_off
          restore_cursor(:index)
          update_title
        end

        def post_toggle_index_mode
          @canvas.activate("ClearGraffiti")
          update_menu
          @area.queue_draw
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

        def pre_print(slide_size)
          start_progress(slide_size)
        end

        def printing(i)
          update_progress(i)
          continue = !@canvas.quitted?
          continue
        end

        def post_print(canceled)
          end_progress
        end

        def pre_to_pixbuf(slide_size)
          start_progress(slide_size)
          @pixbufing_size = [width, height]
        end

        def to_pixbufing(i)
          update_progress(i)
          continue = @pixbufing_size == [width, height] &&
            !@canvas.quitted? && !@canvas.applying?
          continue
        end

        def post_to_pixbuf(canceled)
          end_progress
        end

        def cache_all_slides
          pre_cache_all_slides(@canvas.slide_size)
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

        def pre_cache_all_slides(slide_size)
          @caching = true
          @caching_size = [width, height]
          start_progress(slide_size)
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
          confirm_dialog(message) == Gtk::MessageDialog::RESPONSE_OK
        end

        def reload_theme(&callback)
          if @canvas.applying?
            @need_reload_theme = true
          else
            @canvas.activate("ReloadTheme", &Utils.process_pending_events_proc)
            clear_compiled_slides
          end
        end

        def reload_source(&callback)
          if @canvas.need_reload_source?
            callback ||= Utils.process_pending_events_proc
            super(callback)
          end
        end

        def display?
          true
        end

        def toggle_white_out
          super
          update_menu
          @area.queue_draw
        end

        def toggle_black_out
          super
          update_menu
          @area.queue_draw
        end

        def reset_adjustment
          super
          @area.queue_draw
        end

        def post_init_gui
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

        def update_title
          @canvas.update_title(@canvas.slide_title)
        end

        def init_drawing_area
          @area = Gtk::DrawingArea.new
          @area.can_focus = true
          event_mask = Gdk::Event::BUTTON_PRESS_MASK
          event_mask |= Gdk::Event::BUTTON_RELEASE_MASK
          event_mask |= Gdk::Event::BUTTON1_MOTION_MASK
          event_mask |= Gdk::Event::BUTTON2_MOTION_MASK
          event_mask |= Gdk::Event::BUTTON3_MOTION_MASK
          @area.add_events(event_mask)
          set_realize
          set_key_press_event(@area)
          set_button_event(@area)
          set_motion_notify_event
          set_expose_event
          set_scroll_event
          set_configure_event_after
        end

        def set_realize
          @area.signal_connect_after("realize") do |widget|
            realized(widget)
          end
        end

        def realized(widget)
          @drawable = widget.window
          @white = Gdk::GC.new(@drawable)
          @white.set_rgb_fg_color(Color.parse("white").to_gdk_color)
          @black = Gdk::GC.new(@drawable)
          @black.set_rgb_fg_color(Color.parse("black").to_gdk_color)
          init_renderer(@drawable)
        end

        def set_motion_notify_event
          @area.signal_connect("motion_notify_event") do |widget, event|
            call_hook_procs(@motion_notify_hook_procs, event)
          end
        end

        def set_expose_event
          @area.signal_connect("expose_event") do |widget, event|
            unless @caching
              reload_source
            end

            if @white_out
              @drawable.draw_rectangle(@white, true, 0, 0,
                                       original_width, original_height)
            elsif @black_out
              @drawable.draw_rectangle(@black, true, 0, 0,
                                       original_width, original_height)
            else
              draw_current_slide
              draw_graffiti
              draw_gesture
            end
            true
          end
        end

        def draw_current_slide
          slide = @canvas.current_slide
          if slide
            unless compiled_slide?(slide)
              compile_slide(slide)
            end
            slide.draw(@canvas, false)
          end
        end

        def draw_current_slide_pixbuf(pixbuf)
          width, height = pixbuf.width, pixbuf.height
          x = @adjustment_x * width
          y = @adjustment_y * height
          @drawable.draw_pixbuf(@foreground, pixbuf,
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

          @drawable.draw_pixbuf(@foreground, pixbuf, src_x, src_y,
                                dest_x, dest_y, src_width, src_height,
                                Gdk::RGB::DITHER_NORMAL, 0, 0)
        end

        def set_configure_event_after
          @area.signal_connect_after("configure_event") do |widget, event|
            @mask = nil
            set_hole
            if !@caching and @drawable
              if @canvas.applying?
                @need_reload_theme = true
              else
                reload_theme
              end
            end
            false
          end
        end

        def set_scroll_event
          @area.signal_connect("scroll_event") do |widget, event|
            case event.direction
            when Gdk::EventScroll::Direction::UP
              @canvas.activate("PreviousSlide")
            when Gdk::EventScroll::Direction::DOWN
              @canvas.activate("NextSlide")
            end
          end
        end

        def set_configure_event
          id = @window.signal_connect("configure_event") do |widget, event|
            configured(event.x, event.y, event.width, event.height)
            false
          end
          @configure_signal_id = id
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

        def call_hook_procs(procs, *args)
          procs.find do |proc|
            proc.call(*args)
          end
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
