require 'gtk2'

require "rabbit/utils"
require "rabbit/comment/log"
require "rabbit/renderer/base"
require "rabbit/renderer/engine"
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
      class DrawingArea
        include Base
        include Renderer::Engine::GDK

        include Graffiti
        include Mask
        include Progress
        include Cursor
        include Search
        include Gesture
        include Menu
        include KeyHandler
        include ButtonHandler

        attr_accessor :filename
        def initialize(canvas)
          super
          @filename = nil
          @caching = nil
          @comment_initialized = false
          @need_reload_theme = false
          clear_compiled_slides
          init_drawing_area
          init_comment_log
        end

        def attach_to(window)
          init_menu
          init_gesture_actions
          @window = window
          @hbox = Gtk::HBox.new
          @vbox = Gtk::VBox.new
          @vbox.pack_start(@area, true, true, 0)
          @area.show
          @vbox.pack_end(@comment_log.widget, false, false, 0)
          @hbox.pack_end(@vbox, true, true, 0)
          init_comment_view_canvas
          init_comment_view_frame
          @hbox.pack_start(@comment_view_frame.window, false, false, 0)
          @window.add(@hbox)
          @hbox.show
          @vbox.show
          @menu.attach(window)
          @window.add_accel_group(@accel_group)
          set_configure_event
        end

        def detach_from(window)
          detach_menu(window)
          window.remove(@area)
          window.signal_handler_disconnect(@configure_signal_id)
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

        def destroy
          @area.destroy
        end

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

        def pre_cache_all_slides(slide_size)
          @caching = true
          @caching_size = [width, height]
          start_progress(slide_size)
        end

        def caching_all_slides(i, canvas)
          update_progress(i)
          #         unless @pixmap.has_key?(@canvas.slides[i])
          #           @pixmap[@canvas.slides[i]] = canvas.renderer[canvas.slides[i]]
          #         end
          continue = @caching_size == [width, height] &&
            !@canvas.quitted? && !@canvas.applying?
          continue
        end

        def post_cache_all_slides(canvas, canceled)
          end_progress
          @caching = false
          return if @canvas.quitted?
          if canceled
            reload_theme
          else
            clear_compiled_slides
            #           @canvas.slides.each_with_index do |slide, i|
            #             @pixbufs[slide] = canvas.renderer[canvas.slides[i]]
            #           end
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

        def showing_comment_frame?
          @comment_frame and @comment_frame.visible?
        end

        def showing_comment_view?
          @comment_log.showing?
        end

        def comment_frame_available?
          true
        end

        def comment_view_available?
          true
        end

        def toggle_comment_frame
          ensure_comment
          if showing_comment_frame?
            @comment_frame.hide
          else
            adjust_comment_frame
            @comment_frame.show
          end
          update_menu
        end

        def toggle_comment_view
          ensure_comment
          if showing_comment_view?
            @comment_log.hide
            @comment_view_frame.hide
          else
            adjust_comment_view
            @comment_log.show
            @comment_view_frame.show
            @comment_view_canvas.parse_rd(@canvas.comment_source)
            @comment_view_canvas.move_to_last
          end
          adjust_comment_frame
        end

        def update_comment(source, &block)
          ensure_comment
          error_occurred = parse_comment(source, &block)
          unless error_occurred
            @comment_canvas.move_to_last
            @comment_log.reset(@comment_canvas)
            if @comment_view_frame.visible?
              @comment_view_frame.parse_rd(source)
              @comment_view_canvas.move_to_last
            end
          end
        end

        def reset_adjustment
          super
          @area.queue_draw
        end

        def post_init_gui
          @comment_log.hide
          @comment_view_frame.hide if @comment_view_frame
        end

        private
        def init_dpi
          @x_dpi = ScreenInfo.screen_x_resolution
          @y_dpi = ScreenInfo.screen_y_resolution
        end

        def parse_comment(source)
          error_occurred = false
          @comment_canvas.parse_rd(source) do |error|
            error_occurred = true
            if block_given?
              yield(error)
            else
              @comment_canvas.logger.warn(error)
            end
          end
          error_occurred
        end

        def ensure_comment
          unless @comment_initialized
            init_comment
            @comment_initialized = true
          end
        end

        def init_comment
          init_comment_canvas
          init_comment_frame
          @comment_canvas.parse_rd(@canvas.comment_source)
        end

        def init_comment_frame
          @comment_frame = Frame.new(@comment_canvas.logger, @comment_canvas)
          w, h = suggested_comment_frame_size
          @comment_frame.init_gui(w, h, false, Gtk::Window::POPUP)
          @comment_frame.hide
        end

        def init_comment_canvas
          @comment_canvas = Canvas.new(@canvas.logger, DrawingArea)
        end

        def init_comment_view_frame
          args = [@comment_view_canvas.logger, @comment_view_canvas]
          @comment_view_frame = EmbedFrame.new(*args)
          @comment_view_frame.init_gui(-1, -1, false)
          @comment_view_frame.hide
        end

        def init_comment_view_canvas
          @comment_view_canvas = CommentCanvas.new(@canvas.logger,
                                                   CommentDrawingArea)
          @comment_view_canvas.saved_image_basename = @canvas.saved_image_basename
          @comment_view_canvas.filename = @canvas.filename
          @comment_view_canvas.slides_per_page = @canvas.slides_per_page
        end

        def update_title
          @canvas.update_title(@canvas.slide_title)
        end

        def init_comment_log
          @comment_log = Comment::Log.new
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
          @foreground = Gdk::GC.new(@drawable)
          @background = Gdk::GC.new(@drawable)
          @background.set_foreground(widget.style.bg(Gtk::STATE_NORMAL))
          @white = Gdk::GC.new(@drawable)
          @white.set_rgb_fg_color(Color.parse("white").to_gdk_color)
          @black = Gdk::GC.new(@drawable)
          @black.set_rgb_fg_color(Color.parse("black").to_gdk_color)
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

        def draw_slide(slide, simulation)
          unless simulation
            draw_rectangle(true, 0, 0, width, height, @background)
          end
          yield
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

        def configured(x, y, w, h)
          adjust_comment_frame(x, y, w, h)
          adjust_comment_view(x, y, w, h)
          adjust_search_window
          adjust_progress_window
        end

        def calc_slide_number(val, modifier)
          val += 10 if modifier.control_mask?
          val += 20 if modifier.mod1_mask?
          val
        end

        def adjust_comment_frame(x=nil, y=nil, w=nil, h=nil)
          if @comment_initialized
            w, h = suggested_comment_frame_size(w, h)
            @comment_frame.window.set_size_request(w, h)
            Utils.move_to_bottom_left(@window, @comment_frame.window)
          end
        end

        def adjust_comment_view(x=nil, y=nil, w=nil, h=nil)
          ww, wh = suggested_comment_log_window_size(w, h)
          @comment_log.widget.set_size_request(ww, wh)
          begin
            header_height = @comment_log.header_height
          rescue TypeError
            header_height = nil
          end
          if header_height
            text_size = (wh - header_height) * 0.5
          else
            text_size = wh * 0.4
          end
          @comment_log.font_size = text_size * Pango::SCALE

          fw, fh = suggested_comment_view_frame_size(w, h)
          @comment_view_frame.set_size_request(fw, fh)
        end

        def suggested_comment_frame_size(w=nil, h=nil)
          w ||= @canvas.width
          h ||= @canvas.height
          [w / 10, h / 10]
        end

        def suggested_comment_log_window_size(w=nil, h=nil)
          h ||= @canvas.height
          [-1, h / 10]
        end

        def suggested_comment_view_frame_size(w=nil, h=nil)
          w ||= @canvas.width
          [w / 10, -1]
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
