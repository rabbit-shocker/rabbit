require "rabbit/menu"
require "rabbit/keys"
require "rabbit/renderer/base"

module Rabbit
  module Renderer
    
    class DrawingArea
      include Keys

      BUTTON_PRESS_ACCEPTING_TIME = 0.5 * 1000

      @@color_table = {}
  
      def initialize(canvas)
        @canvas = canvas
        @current_cursor = nil
        @blank_cursor = nil
        @font_families = nil
        clear_button_handler
        init_drawing_area
        update_menu
      end
      
      def attach_to(window)
        window.add(@area)
      end
    
      def foreground=(color)
        @foreground.set_foreground(color)
      end
      
      def background=(color)
        @background.set_foreground(color)
        @drawable.background = color
      end
      
      def background_image=(pixbuf)
        # @background.set_foreground(color)
        @drawable.background_pixbuf = pixbuf
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
        update_menu
        @area.queue_draw
      end
      
      def post_move(index)
        @area.queue_draw
      end
      
      def each_page_pixbuf
        args = [@drawable, 0, 0, width, height]
        before_page_index = @canvas.current_index
        @canvas.pages.each_with_index do |page, i|
          @canvas.move_to_if_can(i)
          @drawable.process_updates(true)
          yield(Gdk::Pixbuf.from_drawable(@drawable.colormap, *args), i)
        end
        @canvas.move_to_if_can(before_page_index)
      end
      
      def post_fullscreen
        set_cursor(blank_cursor)
        update_menu
      end
      
      def post_unfullscreen
        set_cursor(nil)
        update_menu
      end
      
      def post_iconify
        update_menu
      end
      
      def redraw
        @drawing_area.queue_draw
      end
      
      
      def post_parse_rd
        clear_button_handler
        update_menu
      end
      
      def index_mode_on
        @drawable.cursor = @current_cursor
      end
      
      def index_mode_off
        @drawable.cursor = nil
      end
      
      def post_toggle_index_mode
        update_menu
        @area.queue_draw
      end
      
      def font_families
        if @font_families.nil? or @font_families.empty?
          layout = @area.create_pango_layout("")
          @font_families = layout.context.list_families
        end
        @font_families
      end

      def draw_page
        yield
      end
      
      def draw_line(x1, y1, x2, y2, color=nil)
        gc = make_gc(color)
        @drawable.draw_line(gc, x1, y1, x2, y2)
      end
      
      def draw_rectangle(filled, x1, y1, x2, y2, color=nil)
        gc = make_gc(color)
        @drawable.draw_rectangle(gc, filled, x1, y1, x2, y2)
      end
      
      def draw_arc(filled, x, y, w, h, a1, a2, color=nil)
        gc = make_gc(color)
        @drawable.draw_arc(gc, filled, x, y, w, h, a1, a2)
      end
      
      def draw_circle(filled, x, y, w, h, color=nil)
        draw_arc(filled, x, y, w, h, 0, 360 * 64, color)
      end
      
      def draw_layout(layout, x, y, color=nil)
        gc = make_gc(color)
        @drawable.draw_layout(gc, x, y, layout)
      end
      
      def draw_pixbuf(pixbuf, x, y, params={})
        gc = make_gc(params['color'])
        args = [0, 0, x, y,
          params['width'] || pixbuf.width,
          params['height'] || pixbuf.height,
          params['dither_mode'] || Gdk::RGB::DITHER_NORMAL,
          params['x_dither'] || 0,
          params['y_dither'] || 0]
        @drawable.draw_pixbuf(gc, pixbuf, *args)
      end
      
      def make_color(color, default_is_foreground=true)
        make_gc(color, default_is_foreground).foreground
      end

      def make_layout(text)
        attrs, text = Pango.parse_markup(text)
        layout = @area.create_pango_layout(text)
        layout.set_attributes(attrs)
        w, h = layout.size.collect {|x| x / Pango::SCALE}
        [layout, w, h]
      end

      private
      def clear_button_handler
        @button_handler_thread = nil
        @button_handler = []
      end
    
      def update_menu
        @menu = Menu.new(@canvas)
      end
      
      def make_gc(color, default_is_foreground=true)
        if color.nil?
          if default_is_foreground
            @foreground
          else
            @background
          end
        else
          make_gc_from_string(color)
        end
      end

      def make_gc_from_string(str)
        gc = Gdk::GC.new(@drawable)
        if @@color_table.has_key?(str)
          color = @@color_table[str]
        else
          color = Gdk::Color.parse(str)
          colormap = Gdk::Colormap.system
          unless colormap.alloc_color(color, false, true)
            raise CantAllocateColorError.new(str)
          end
          @@color_table[str] = color
        end
        gc.set_foreground(color)
        gc
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
          page = @canvas.current_page
          if page
            page.draw(@canvas)
#             if next_page
#               next_page.draw(self, true)
#             end
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
      
      def calc_page_number(key_event, base)
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
          index = calc_page_number(key_event, Gdk::Keyval::GDK_0)
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
          index = calc_page_number(key_event, Gdk::Keyval::GDK_KP_0)
          @canvas.move_to_if_can(index)
        when *TOGGLE_FULLSCREEN_KEYS
          @canvas.toggle_fullscreen
          @canvas.reload_theme
        when *RELOAD_THEME_KEYS
          @canvas.reload_theme
        when *SAVE_AS_IMAGE_KEYS
          @canvas.save_as_image
        when *ICONIFY_KEYS
          @canvas.iconify
        when *TOGGLE_INDEX_MODE_KEYS
          @canvas.toggle_index_mode
        end
      end
      
      def handle_key_with_control(key_event)
        handled = false
        case key_event.keyval
        when *Control::REDRAW_KEYS
          @canvas.redraw
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
            index = @canvas.current_page.page_number(@canvas, event.x, event.y)
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
      
    end
    
  end
end
