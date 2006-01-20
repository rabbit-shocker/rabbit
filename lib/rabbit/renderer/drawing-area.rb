require "forwardable"

require "rabbit/menu"
require "rabbit/keys"
require "rabbit/graffiti"
require "rabbit/renderer/pixmap"

module Rabbit
  module Renderer
    class DrawingArea
      include Base

      extend Forwardable

      @@color_table = {}
      
      def_delegators(:@pixmap, :foreground, :background)
      def_delegators(:@pixmap, :foreground=, :background=)
      def_delegators(:@pixmap, :background_image, :background_image=)
      
      def_delegators(:@pixmap, :draw_slide, :draw_line, :draw_rectangle)
      def_delegators(:@pixmap, :draw_arc, :draw_circle, :draw_layout)
      def_delegators(:@pixmap, :draw_arc_by_radius, :draw_circle_by_radius)
      def_delegators(:@pixmap, :draw_pixbuf, :draw_polygon)
      def_delegators(:@pixmap, :draw_rounded_rectangle)

      def_delegators(:@pixmap, :draw_cube, :draw_sphere, :draw_cone)
      def_delegators(:@pixmap, :draw_torus, :draw_tetrahedron)
      def_delegators(:@pixmap, :draw_octahedron, :draw_dodecahedron)
      def_delegators(:@pixmap, :draw_icosahedron, :draw_teapot)
      
      def_delegators(:@pixmap, :gl_compile, :gl_call_list)
    
      def_delegators(:@pixmap, :make_color)

      def_delegators(:@pixmap, :to_pixbuf)

      def_delegators(:@pixmap, :clear_pixmap, :clear_pixmaps)

      def_delegators(:@pixmap, :filename, :filename=)
      
      def_delegators(:@pixmap, :x_dpi, :y_dpi)
      
      BUTTON_PRESS_ACCEPTING_TIME = 250
      MASK_SIZE_STEP = 0.05

      def initialize(canvas)
        @progress = nil
        super
        @current_cursor = nil
        @blank_cursor = nil
        @pencil_cursor = nil
        @caching = nil
        @comment_initialized = false
        @button_handling = false
        @mask = nil
        @mask_size = 0
        @need_reload_theme = false
        init_progress
        clear_button_handler
        init_graffiti
        init_drawing_area
        init_pixmap(1, 1)
        init_comment_log_window
      end
      
      def attach_to(window)
        init_menu
        @window = window
        @hbox = Gtk::HBox.new
        @vbox = Gtk::VBox.new
        @vbox.pack_start(@area, true, true, 0)
        @area.show
        @vbox.pack_end(@comment_log_window, false, false, 0)
        @hbox.pack_end(@vbox, true, true, 0)
        init_comment_view_canvas
        init_comment_view_frame
        @hbox.pack_start(@comment_view_frame.window, false, false, 0)
        @window.add(@hbox)
        @hbox.show
        @vbox.show
        @window.add_accel_group(@menu.accel_group)
        set_configure_event
      end
    
      def detach_from(window)
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
          @pixmap.post_apply_theme
          update_menu
          @area.queue_draw
        end
      end
      
      def post_move(index)
        @pixmap.post_move(index)
        update_title
        reset_adjustment
        clear_graffiti
        # toggle_graffiti_mode if @graffiti_mode
        @area.queue_draw
      end
      
      def post_fullscreen
        update_cursor(blank_cursor, true)
        clear_pixmaps
        update_menu
      end
      
      def post_unfullscreen
        update_cursor(nil, true)
        clear_pixmaps
        update_menu
      end
      
      def post_iconify
        update_menu
      end
      
      def redraw
        clear_pixmap
        @area.queue_draw
      end
      
      def pre_parse_rd
        update_menu
        clear_keys
        @pixmap.pre_parse_rd
      end
      
      def post_parse_rd
        clear_button_handler
        update_title
        update_menu
        @pixmap.post_parse_rd
        if @need_reload_theme
          @need_reload_theme = false
          reload_theme
        end
      end
      
      def index_mode_on
        @before_index_mode_cursor = @current_cursor
        update_cursor(nil, true)
      end
      
      def index_mode_off
        update_cursor(@before_index_mode_cursor, true)
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
        unless @pixmap.has_key?(@canvas.slides[i])
          @pixmap[@canvas.slides[i]] = canvas.renderer[canvas.slides[i]]
        end
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
          @pixmap.clear_pixmaps
          @canvas.slides.each_with_index do |slide, i|
            @pixmap[slide] = canvas.renderer[canvas.slides[i]]
          end
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
          @canvas.activate("ReloadTheme") {Utils.process_pending_events_proc}
          clear_pixmaps
        end
      end

      def reload_source(&callback)
        if @canvas.need_reload_source?
          callback ||= Utils.process_pending_events_proc
          super(callback)
        end
      end

      def progress_foreground=(color)
        super
        setup_progress_color
      end
      
      def progress_background=(color)
        super
        setup_progress_color
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
        @comment_log_window and @comment_log_window.visible?
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
          @comment_log_window.hide_all
          @comment_view_frame.hide
        else
          adjust_comment_view
          @comment_log_window.show_all
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
          reset_comment_log
          if @comment_view_frame.visible?
            @comment_view_frame.parse_rd(source)
            @comment_view_canvas.move_to_last
          end
        end
      end
      
      def expand_hole
        if @mask_size < 0
          @mask_size = MASK_SIZE_STEP
        else
          @mask_size = [@mask_size + MASK_SIZE_STEP, 1.0].min
        end
        set_hole
      end

      def narrow_hole
        if @mask_size < 0
          @mask_size = 0
        else
          @mask_size = [@mask_size - MASK_SIZE_STEP, 0.0].max
        end
        set_hole
      end

      def set_hole
        if @mask_size <= 0
          @window.shape_combine_mask(nil, 0, 0)
        else
          setup_mask if @mask.nil?
          w, h = width, height
          @mask.draw_rectangle(@set_gc, true, 0, 0, w, h)
          mw = w * @mask_size
          mh = h * @mask_size
          mx = (w - mw) / 2
          my = (h - mh) / 2
          @mask.draw_rectangle(@xor_gc, true, mx, my, mw, mh)
          @window.shape_combine_mask(@mask, 0, 0)
        end
        update_title # for xfwm
      end

      def graffiti_mode?
        @graffiti_mode
      end

      def have_graffiti?
        @graffiti.have_graffiti?
      end
      
      def can_undo_graffiti?
        @graffiti.can_undo?
      end
      
      def toggle_graffiti_mode
        @graffiti_mode = !@graffiti_mode
        update_cursor(@current_cursor)
        update_menu
      end

      def clear_graffiti
        @graffiti.clear
        Action.update_graffiti_action_status(@canvas)
        @area.queue_draw
      end

      def undo_graffiti
        @graffiti.undo
        Action.update_graffiti_action_status(@canvas)
        @area.queue_draw
      end
      
      def reset_adjustment
        super
        @area.queue_draw
      end

      def post_init_gui
        @comment_log_window.hide
        @comment_view_frame.hide if @comment_view_frame
      end

      def clear_theme
        @pixmap.clear_theme
        super
      end
      
      private
      def init_pixmap(w=width, h=height)
        @pixmap = Renderer::Pixmap.new(@canvas, w, h)
        @pixmap.setup_event(self)
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
      
      def clear_button_handler
        @button_handler = []
      end

      def clear_keys
        @keys = Keys.new
      end
      
      def clear_progress_color
        super
        setup_progress_color
      end

      def update_menu
        @menu.update_menu(@canvas)
      end

      def update_title
        @canvas.update_title(@canvas.slide_title)
      end

      def init_menu
        @menu = Menu.new(@canvas.actions)
      end
      
      def init_progress
        @progress_window = Gtk::Window.new(Gtk::Window::POPUP)
        @progress_window.app_paintable = true
        @progress = Gtk::ProgressBar.new
        @progress.show_text = true
        @progress_window.add(@progress)
      end

      def setup_progress_color
        return unless @progress
        style = @progress.style.copy
        if @progress_foreground
          rgb = @pixmap.to_gdk_rgb(@progress_foreground)
          style.set_bg(Gtk::STATE_NORMAL, *rgb)
        end
        if @progress_background
          rgb = @pixmap.to_gdk_rgb(@progress_background)
          style.set_bg(Gtk::STATE_PRELIGHT, *rgb)
        end
        @progress.style = style
      end


      def init_graffiti
        @graffiti = Graffiti.new
        @graffiti_mode = false

        pressed_button = nil
        target_button = 1
        
        add_button_press_hook do |event|
          pressed_button = event.button
          if @graffiti_mode and event.button == target_button
            @graffiti.button_press(event.x, event.y, width, height)
            true
          else
            false
          end
        end
        
        add_button_release_hook do |event, last_button_press_event|
          pressed_button = nil
          if @graffiti_mode and event.button == target_button
            @graffiti.button_release(event.x, event.y, width, height)
            Action.update_graffiti_action_status(@canvas)
            true
          else
            false
          end
        end
        
        add_motion_notify_hook do |event|
          if @graffiti_mode and
              @graffiti.dragging? and
              pressed_button == target_button
            @graffiti.button_motion(event.x, event.y, width, height)
            @graffiti.draw_last_segment(@drawable, @foreground)
            true
          else
            false
          end
        end
      end
      
      
      COMMENT_LOG_COMMENT_COLUMN = 0

      def reset_comment_log
        @comment_log_model.clear
        @comment_canvas.slides[1..-1].each do |slide|
          iter = @comment_log_model.prepend
          iter.set_value(COMMENT_LOG_COMMENT_COLUMN, slide.headline.text)
        end
      end
      
      def init_comment_log_model
        @comment_log_model = Gtk::ListStore.new(String)
      end

      def init_comment_log_view
        init_comment_log_model
        @comment_log_view = Gtk::TreeView.new(@comment_log_model)
        @comment_log_view.can_focus = false
        @comment_log_view.rules_hint = true
        @comment_log_renderer_comment = Gtk::CellRendererText.new
        args = [
          _("comment"),
          @comment_log_renderer_comment,
          {"text" => COMMENT_LOG_COMMENT_COLUMN}
        ]
        @comment_log_column_comment = Gtk::TreeViewColumn.new(*args)
        @comment_log_view.append_column(@comment_log_column_comment)
      end
      
      def init_comment_log_window
        init_comment_log_view
        @comment_log_window = Gtk::ScrolledWindow.new
        @comment_log_window.set_policy(Gtk::POLICY_AUTOMATIC,
                                       Gtk::POLICY_AUTOMATIC)
        @comment_log_window.add(@comment_log_view)
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
        set_key_press_event
        set_button_event
        set_motion_notify_event
        set_expose_event
        set_scroll_event
        set_configure_event_after
      end
      
      def set_realize
        @area.signal_connect_after("realize") do |widget|
          @drawable = widget.window
          @foreground = Gdk::GC.new(@drawable)
          @background = Gdk::GC.new(@drawable)
          @background.set_foreground(widget.style.bg(Gtk::STATE_NORMAL))
          @white = Gdk::GC.new(@drawable)
          @white.set_foreground(@pixmap.make_gdk_color("white"))
          @black = Gdk::GC.new(@drawable)
          @black.set_foreground(@pixmap.make_gdk_color("black"))
          init_pixmap
        end
      end
      
      def set_key_press_event
        @area.signal_connect("key_press_event") do |widget, event|
          handled = false

          if event.state.control_mask?
            handled = handle_key_with_control(event)
          end
          
          if event.state.mod1_mask?
            handled = handle_key_with_alt(event)
          end
          
          unless handled
            handled = handle_key(event)
          end
          
          handled
        end
      end

      def set_button_event
        last_button_press_event = nil
        @area.signal_connect("button_press_event") do |widget, event|
          last_button_press_event = event
          call_hook_procs(@button_press_hook_procs, event)
        end

        @area.signal_connect("button_release_event") do |widget, event|
          handled = call_hook_procs(@button_release_hook_procs,
                                    event, last_button_press_event)
          if handled
            clear_button_handler
          else
            handled = handle_button_release(event, last_button_press_event)
          end
          handled
        end
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
            @graffiti.draw_all_segment(@drawable, @foreground)
          end
        end
      end

      def draw_current_slide
        slide = @canvas.current_slide
        if slide
          unless @pixmap.has_key?(slide)
            @pixmap.width = width
            @pixmap.height = height
            slide.draw(@canvas)
          end
          pixmap = @pixmap[slide]
          if pixmap
            if block_given?
              yield(pixmap)
            else
              draw_pixmap(pixmap)
            end
          end
        end
      end
      
      def draw_pixmap(pixmap)
        width, height = pixmap.size
        x = @adjustment_x * width
        y = @adjustment_y * height
        @drawable.draw_drawable(@foreground, pixmap,
                                x, y, 0, 0, width, height)
        if @adjustment_x != 0 or @adjustment_y != 0
          draw_next_slide
        end
      end

      def draw_next_slide
        @canvas.change_current_index(@canvas.current_index + 1) do
          draw_current_slide do |pixmap|
            draw_next_pixmap(pixmap)
          end
        end
      end

      def draw_next_pixmap(pixmap)
        width, height = pixmap.size
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

        @drawable.draw_drawable(@foreground, pixmap, src_x, src_y,
                                dest_x, dest_y, src_width, src_height)

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
          args = [event.x, event.y, event.width, event.height]
          adjust_comment_frame(*args)
          adjust_comment_view(*args)
          adjust_progress_window(*args)
          false
        end
        @configure_signal_id = id
      end
      
      def update_cursor(cursor, update_current_cursor=false)
        if @graffiti_mode
          @drawable.cursor = pencil_cursor
        else
          @drawable.cursor = cursor
        end
        @current_cursor = cursor if update_current_cursor
      end

      def blank_cursor
        if @blank_cursor.nil?
          source = Gdk::Pixmap.new(@drawable, 1, 1, 1)
          mask = Gdk::Pixmap.new(@drawable, 1, 1, 1)
          fg = @foreground.foreground
          bg = @background.foreground
          @blank_cursor = Gdk::Cursor.new(source, mask, fg, bg, 1, 1)
        end
        @blank_cursor
      end

      def pencil_cursor
        @pencil_cursor ||= Gdk::Cursor.new(Gdk::Cursor::PENCIL)
      end
      
      def calc_slide_number(key_event, base)
        val = key_event.keyval
        val += 10 if key_event.state.control_mask?
        val += 20 if key_event.state.mod1_mask?
        val - base
      end

      def handle_key(key_event)
        handled = true
        case key_event.keyval
        when *@keys.quit_keys
          @canvas.activate("Quit")
        when *@keys.move_to_next_keys
          @canvas.activate("NextSlide")
        when *@keys.move_to_previous_keys
          @canvas.activate("PreviousSlide")
        when *@keys.move_to_first_keys
          @canvas.activate("FirstSlide")
        when *@keys.move_to_last_keys
          @canvas.activate("LastSlide")
        when Gdk::Keyval::GDK_0,
          Gdk::Keyval::GDK_1,
          Gdk::Keyval::GDK_2,
          Gdk::Keyval::GDK_3,
          Gdk::Keyval::GDK_4,
          Gdk::Keyval::GDK_5,
          Gdk::Keyval::GDK_6,
          Gdk::Keyval::GDK_7,
          Gdk::Keyval::GDK_8,
          Gdk::Keyval::GDK_9
          index = calc_slide_number(key_event, Gdk::Keyval::GDK_0)
          @canvas.activate("JumpTo") {index}
        when Gdk::Keyval::GDK_KP_0,
          Gdk::Keyval::GDK_KP_1,
          Gdk::Keyval::GDK_KP_2,
          Gdk::Keyval::GDK_KP_3,
          Gdk::Keyval::GDK_KP_4,
          Gdk::Keyval::GDK_KP_5,
          Gdk::Keyval::GDK_KP_6,
          Gdk::Keyval::GDK_KP_7,
          Gdk::Keyval::GDK_KP_8,
          Gdk::Keyval::GDK_KP_9
          index = calc_slide_number(key_event, Gdk::Keyval::GDK_KP_0)
          @canvas.activate("JumpTo") {index}
        when *@keys.toggle_fullscreen_keys
          @canvas.activate("ToggleFullScreen")
        when *@keys.reload_theme_keys
          reload_theme
        when *@keys.save_as_image_keys
          @canvas.activate("SaveAsImage")
        when *@keys.iconify_keys
          @canvas.activate("Iconify")
        when *@keys.toggle_index_mode_keys
          @canvas.activate("ToggleIndexMode")
        when *@keys.cache_all_slides_keys
          @canvas.activate("CacheAllSlides")
        when *@keys.white_out_keys
          @canvas.activate("ToggleWhiteOut")
        when *@keys.black_out_keys
          @canvas.activate("ToggleBlackOut")
        when *@keys.toggle_comment_frame_keys
          @canvas.activate("ToggleCommentFrame")
        when *@keys.toggle_comment_view_keys
          @canvas.activate("ToggleCommentView")
        when *@keys.expand_hole_keys
          @canvas.activate("ExpandHole")
        when *@keys.narrow_hole_keys
          @canvas.activate("NarrowHole")
        when *@keys.toggle_graffiti_mode_keys
          @canvas.activate("ToggleGraffitiMode")
        else
          handled = false
        end
        handled
      end
      
      def handle_key_when_processing(key_event)
        case key_event.keyval
        when *@keys.quit_keys
          @canvas.activate("Quit")
        else
          @canvas.logger.warn(_("Processing..."))
        end
      end
      
      def handle_key_with_control(key_event)
        handled = true
        if @graffiti_mode
          case key_event.keyval
          when *@keys.control.clear_graffiti_keys
            @canvas.activate("ClearGraffiti")
          when *@keys.control.undo_graffiti_keys
            @canvas.activate("UndoGraffiti")
          else
            handled = false
          end
        else
          case key_event.keyval
          when *@keys.control.redraw_keys
            @canvas.activate("Redraw")
          when *@keys.control.print_keys
            @canvas.activate("Print")
          else
            handled = false
          end
        end
        handled
      end
      
      def handle_key_with_alt(key_event)
        handled = true
        case key_event.keyval
        when *@keys.alt.reset_adjustment_keys
          @canvas.activate("ResetAdjustment")
        else
          handled = false
        end
        handled
      end
      
      BUTTON_PRESS_HANDLER = {
        Gdk::Event::Type::BUTTON_PRESS => "handle_button_press",
        Gdk::Event::Type::BUTTON2_PRESS => "handle_button2_press",
        Gdk::Event::Type::BUTTON3_PRESS => "handle_button3_press",
      }
      
      def handle_button_release(event, last_button_press_event)
        press_event_type = last_button_press_event.event_type
        if BUTTON_PRESS_HANDLER.has_key?(press_event_type)
          __send__(BUTTON_PRESS_HANDLER[press_event_type],
                   last_button_press_event, event)
          start_button_handler
        end
        true
      end

      def handle_button_press(event, release_event)
        case event.button
        when 1, 5
          unless release_event.state.mod1_mask?
            add_button_handler do
              @canvas.activate("NextSlide")
            end
          end
        when 2, 4
          unless release_event.state.mod1_mask?
            add_button_handler do
              @canvas.activate("PreviousSlide")
            end
          end
        when 3
          add_button_handler do
            @menu.popup(0, Gtk.current_event_time)
          end
        end
      end
      
      def handle_button2_press(event, release_event)
        add_button_handler do
          if @canvas.index_mode?
            index = @canvas.current_slide.slide_number(@canvas, event.x, event.y)
            if index
              @canvas.activate("ToggleIndexMode")
              @canvas.activate("JumpTo") {index}
            end
          end
          clear_button_handler
        end
      end
      
      def handle_button3_press(event, release_event)
        add_button_handler do
          clear_button_handler
        end
      end
      
      def add_button_handler(handler=Proc.new)
        @button_handler.push(handler)
      end
      
      def call_button_handler
        @button_handler.pop.call until @button_handler.empty?
      end
      
      def start_button_handler
        if @button_handling
          @coming = true
        else
          @button_handling = true
          @coming = false
          Gtk.timeout_add(BUTTON_PRESS_ACCEPTING_TIME) do
            if @coming
              Gtk.timeout_add(BUTTON_PRESS_ACCEPTING_TIME) do
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

      def start_progress(max)
        return if max.zero?
        update_menu
        @progress_window.transient_for = @canvas.window
        @progress_window.show_all
        adjust_progress_window
        @progress.fraction = @progress_current = 0
        @progress_max = max.to_f
      end

      def update_progress(i)
        @progress_current = i
        @progress.fraction = @progress_current / @progress_max
        Utils.process_pending_events
      end

      def end_progress
        @progress_current = @progress_max
        Gtk.timeout_add(100) do
          @progress_window.hide
          update_menu
          false
        end
      end

      def adjust_progress_window(x=nil, y=nil, w=nil, h=nil)
        wx, wy = @canvas.window.position
        @progress_window.move(x || wx, y || wy)
      end

      def adjust_comment_frame(x=nil, y=nil, w=nil, h=nil)
        if @comment_initialized
          w, h = suggested_comment_frame_size(w, h)
          @comment_frame.resize(w, h)
          wx, wy = @canvas.window.position
          x ||= wx
          y ||= wy
          if @comment_view_frame.visible?
            x = x + @comment_view_frame.window.size_request[0]
          end
          y = y + height - h
          @comment_frame.window.move(x, y)
        end
      end

      def adjust_comment_view(x=nil, y=nil, w=nil, h=nil)
        ww, wh = suggested_comment_log_window_size(w, h)
        @comment_log_window.set_size_request(ww, wh)
        begin
          _, _, _, header_height = @comment_log_column_comment.cell_size
        rescue TypeError
          header_height = nil
        end
        if header_height
          text_size = (wh - header_height) * 0.5
        else
          text_size = wh * 0.4
        end
        @comment_log_renderer_comment.size = text_size * Pango::SCALE
      
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

      def setup_mask
        @mask = Gdk::Pixmap.new(nil, width, height, 1)
        @xor_gc = Gdk::GC.new(@mask)
        @xor_gc.set_function(Gdk::GC::INVERT)
        @set_gc = Gdk::GC.new(@mask)
        @set_gc.set_function(Gdk::GC::SET)
        @clear_gc = Gdk::GC.new(@mask)
        @clear_gc.set_function(Gdk::GC::CLEAR)
      end
    end

    class CommentDrawingArea < DrawingArea

      attr_accessor :direction

      def width
        original_height
      end

      def height
        original_width
      end
      
      def initialize(canvas)
        super
        @direction = :right
        @pixbufs = {}
      end

      def attach_to(window)
        @window = window
        @window.add(@area)
        @area.show
      end
      
      def clear_pixmap(slide=nil)
        @pixbufs.delete(slide || @canvas.current_slide)
      end

      def clear_pixmaps
        @pixbufs = {}
        super
      end

      def post_apply_theme
        @pixbufs = {}
        super
      end

      def toggle_comment_frame
      end

      def toggle_comment_view
      end

      def showing_comment_frame?
        false
      end

      def showing_comment_view?
        false
      end

      def comment_frame_available?
        false
      end

      def comment_view_available?
        false
      end

      def update_comment(*args, &block)
      end
      
      def post_init_gui
      end

      def post_toggle_index_mode
        @pixbufs = {}
        super
      end
      
      def post_to_pixbuf(canceled)
        super
        @pixbufs = {}
      end

      private
      def init_drawing_area
        super
        @area.can_focus = false
      end

      def init_comment_log_window
      end
      
      def init_comment
      end

      def adjust_comment_frame(*args)
      end

      def adjust_comment_view(*args)
      end

      def adjust_progress_window(*args)
      end

      def draw_pixmap(slide)
        unless @pixbufs.has_key?(slide)
          @pixbufs[slide] = Utils.rotate_pixbuf(@pixmap.to_pixbuf(slide))
        end
        @drawable.draw_pixbuf(@foreground, @pixbufs[slide],
                              0, 0, 0, 0, -1, -1,
                              Gdk::RGB::DITHER_NORMAL, 0, 0)
      end
    end
  end
end
