require "forwardable"

require "rabbit/menu"
require "rabbit/keys"
require "rabbit/renderer/pixmap"

module Rabbit
  module Renderer
    
    class DrawingArea
      include Base
      include Keys

      extend Forwardable
    
      @@color_table = {}
      
      def_delegators(:@pixmap, :foreground, :background)
      def_delegators(:@pixmap, :foreground=, :background=)
      def_delegators(:@pixmap, :background_image, :background_image=)
      
      def_delegators(:@pixmap, :draw_slide, :draw_line, :draw_rectangle)
      def_delegators(:@pixmap, :draw_arc, :draw_circle, :draw_layout)
      def_delegators(:@pixmap, :draw_pixbuf)

      def_delegators(:@pixmap, :make_color)

      def_delegators(:@pixmap, :to_pixbuf)
      
      BUTTON_PRESS_ACCEPTING_TIME = 0.5 * 1000

      attr_accessor :filename
      
      def initialize(canvas)
        super
        @current_cursor = nil
        @blank_cursor = nil
        @filename = nil
        init_progress
        clear_button_handler
        init_drawing_area
        init_pixmap(1, 1)
        update_menu
      end
      
      def attach_to(window)
        window.add(@area)
      end
    
      def detach_from(window)
        window.remove(@area)
      end
    
      def width
        @drawable.size[0]
      end
      
      def height
        @drawable.size[1]
      end
      
      def destroy
        @area.destroy
      end
      
      def post_apply_theme
        @pixmap.post_apply_theme
        update_menu
        @area.queue_draw
      end
      
      def post_move(index)
        @pixmap.post_move(index)
        update_title
        @area.queue_draw
      end
      
      def post_fullscreen
        set_cursor(blank_cursor)
        clear_pixmap
        update_menu
      end
      
      def post_unfullscreen
        set_cursor(nil)
        clear_pixmap
        update_menu
      end
      
      def post_iconify
        update_menu
      end
      
      def redraw
        clear_pixmap
        @area.queue_draw
      end
      
      
      def post_parse_rd
        clear_button_handler
        update_title
        update_menu
      end
      
      def index_mode_on
        @drawable.cursor = nil
      end
      
      def index_mode_off
        @drawable.cursor = @current_cursor
      end
      
      def post_toggle_index_mode
        update_menu
        @area.queue_draw
      end
      
      def make_layout(text)
        attrs, text = Pango.parse_markup(text)
        layout = @area.create_pango_layout(text)
        layout.set_attributes(attrs)
        w, h = layout.size.collect {|x| x / Pango::SCALE}
        [layout, w, h]
      end

      def pango_context
      end
      
      def create_pango_context
        @area.create_pango_context
      end
      
      def pre_print(slide_size)
        start_progress(slide_size)
      end

      def printing(i)
        update_progress(i)
      end

      def post_print
        end_progress
      end

      def pre_to_pixbuf(slide_size)
        start_progress(slide_size)
      end

      def to_pixbufing(i)
        update_progress(i)
      end
      
      def post_to_pixbuf
        end_progress
      end

      private
      def can_create_pixbuf?
        false
      end
      
      def init_pixmap(w=width, h=height)
        @pixmap = Renderer::Pixmap.new(@canvas, w, h)
      end
      
      def clear_button_handler
        @button_handler_thread = nil
        @button_handler = []
      end

      def update_menu
        @menu = Menu.new(@canvas)
      end

      def update_title
        @canvas.update_title(@canvas.slide_title)
      end
      
      def init_progress
        @progress_window = Gtk::Window.new(Gtk::Window::POPUP)
        @progress_window.window_position = Gtk::Window::POS_CENTER_ALWAYS
        @progress_window.app_paintable = true
        @progress = Gtk::ProgressBar.new
        @progress.show_text = true
        @progress_window.add(@progress)
      end
      
      def init_drawing_area
        @area = Gtk::DrawingArea.new
        @area.set_can_focus(true)
        @area.add_events(Gdk::Event::BUTTON_PRESS_MASK)
        set_realize
        set_key_press_event
        set_button_press_event
        set_expose_event
        set_scroll_event
      end
      
      def set_realize
        @area.signal_connect("realize") do |widget, event|
          @drawable = widget.window
          @foreground = Gdk::GC.new(@drawable)
          @background = Gdk::GC.new(@drawable)
          @background.set_foreground(widget.style.bg(Gtk::STATE_NORMAL))
          init_pixmap
        end
      end
      
      def set_key_press_event
        @area.signal_connect("key_press_event") do |widget, event|
          handled = false
          
          if event.state.control_mask?
            handled = handle_key_with_control(event)
          end
          
          unless handled
            handle_key(event)
          end
        end
      end
      
      BUTTON_PRESS_HANDLER = {
        Gdk::Event::Type::BUTTON_PRESS => "handle_button_press",
        Gdk::Event::Type::BUTTON2_PRESS => "handle_button2_press",
        Gdk::Event::Type::BUTTON3_PRESS => "handle_button3_press",
      }
      
      def set_button_press_event
        @area.signal_connect("button_press_event") do |widget, event|
          if BUTTON_PRESS_HANDLER.has_key?(event.event_type)
            __send__(BUTTON_PRESS_HANDLER[event.event_type], event)
            start_button_handler
          end
        end
      end
      
      def set_expose_event
        prev_width = prev_height = nil
        @area.signal_connect("expose_event") do |widget, event|
          @canvas.reload_source
          if @drawable
            if prev_width.nil? or prev_height.nil? or
                [prev_width, prev_height] != [width, height]
              @canvas.reload_theme
              prev_width, prev_height = width, height
            end
          end
          slide = @canvas.current_slide
          if slide
            unless @pixmap.has_key?(slide)
              @pixmap.width = width
              @pixmap.height = height
              slide.draw(@canvas)
            end
            @drawable.draw_drawable(@foreground, @pixmap[slide],
                                    0, 0, 0, 0, -1, -1)
          end
        end
      end
      
      def set_scroll_event
        @area.signal_connect("scroll_event") do |widget, event|
          case event.direction
          when Gdk::EventScroll::Direction::UP
            @canvas.move_to_previous_if_can
          when Gdk::EventScroll::Direction::DOWN
            @canvas.move_to_next_if_can
          end
        end
      end
      
      def set_cursor(cursor)
        @current_cursor = @drawable.cursor = cursor
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
      
      def calc_slide_number(key_event, base)
        val = key_event.keyval
        val += 10 if key_event.state.control_mask?
        val += 20 if key_event.state.mod1_mask?
        val - base
      end

      def handle_key(key_event)
        case key_event.keyval
        when *QUIT_KEYS
          @canvas.quit
        when *MOVE_TO_NEXT_KEYS
          @canvas.move_to_next_if_can
        when *MOVE_TO_PREVIOUS_KEYS
          @canvas.move_to_previous_if_can
        when *MOVE_TO_FIRST_KEYS
          @canvas.move_to_first
        when *MOVE_TO_LAST_KEYS
          @canvas.move_to_last
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
          @canvas.move_to_if_can(index)
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
          @canvas.move_to_if_can(index)
        when *TOGGLE_FULLSCREEN_KEYS
          @canvas.toggle_fullscreen
          @canvas.reload_theme
        when *RELOAD_THEME_KEYS
          @canvas.reload_theme
        when *SAVE_AS_IMAGE_KEYS
          Thread.new do
            @canvas.save_as_image
          end
        when *ICONIFY_KEYS
          @canvas.iconify
        when *TOGGLE_INDEX_MODE_KEYS
          Thread.new do
            @canvas.toggle_index_mode
          end
        end
      end
      
      def handle_key_with_control(key_event)
        handled = false
        case key_event.keyval
        when *Control::REDRAW_KEYS
          @canvas.redraw
          handled = true
        when *Control::PRINT_KEYS
          Thread.new do
            @canvas.print
          end
          handled = true
        end
        handled
      end
      
      def handle_button_press(event)
        case event.button
        when 1, 5
          add_button_handler do
            @canvas.move_to_next_if_can
          end
        when 2, 4
          add_button_handler do
            @canvas.move_to_previous_if_can
          end
        when 3
          @menu.popup(event.button, event.time)
        end
      end
      
      def handle_button2_press(event)
        add_button_handler do
          if @canvas.index_mode?
            index = @canvas.current_slide.slide_number(@canvas, event.x, event.y)
            if index
              @canvas.toggle_index_mode
              @canvas.move_to_if_can(index)
            end
          end
          clear_button_handler
        end
      end
      
      def handle_button3_press(event)
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
        Gtk.timeout_add(BUTTON_PRESS_ACCEPTING_TIME) do
          call_button_handler
          false
        end
      end

      def start_progress(max)
        return if max.zero?
        @progress_window.show_all
        @progress.fraction = @progress_current = 0
        @progress_max = max.to_f
        Gtk.timeout_add(100) do
          @progress.fraction = @progress_current / @progress_max
          @progress_current < @progress_max
        end
      end

      def update_progress(i)
        @progress_current = i
      end

      def end_progress
        @progress_current = @progress_max
        Gtk.timeout_add(100) do
          @progress_window.hide
          false
        end
      end
      
      def clear_pixmap
        @pixmap.clear_pixmaps
      end

    end
    
  end
end
