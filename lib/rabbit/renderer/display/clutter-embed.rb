require 'clutter_gtk'
require 'clutter_cairo'

require "rabbit/utils"
require "rabbit/renderer/engine"
require "rabbit/renderer/display/base"
require 'rabbit/renderer/display/cursor'
require "rabbit/renderer/display/progress"
require "rabbit/renderer/display/mask"
require "rabbit/renderer/display/search"
require "rabbit/renderer/display/gesture"
require "rabbit/renderer/display/graffiti"
require "rabbit/renderer/display/menu"
require "rabbit/renderer/display/button-handler"
require "rabbit/renderer/display/key-handler"
require "rabbit/renderer/display/info"
require "rabbit/renderer/display/spotlight"
require "rabbit/renderer/display/magnifier"

module Rabbit
  add_gui_init_proc do
    Clutter::Gtk.init
  end

  module Renderer
    module Display
      class ClutterEmbed
        class << self
          def priority
            200
          end
        end

        include Base

        include Cursor
        include Graffiti
        include Mask
        include Progress
        include Search
        include Gesture
        include KeyHandler
        include ButtonHandler
        include Info
        include Spotlight
        include Magnifier

        include Renderer::Engine::Cairo

        attr_accessor :filename
        def initialize(canvas)
          @recreate_id = nil
          @need_reload_theme = false
          super
          @filename = nil
          init_clutter_embed
        end

        def attach_to(window)
          @window = window
          @window.add(@embed)
          @embed.show

          init_menu
          init_gesture_actions
          attach_menu
          attach_key
        end

        def detach
          detach_key
          detach_menu
          unless @window.destroyed?
            @window.remove(@embed)
          end

          @embed.hide
          @window = nil
        end

        def widget
          @embed
        end

        def redraw
          @embed.queue_draw
        end

        def clear_slide
          super
          redraw
        end

        def post_fullscreen
          update_cursor(:blank, true)
        end

        def post_unfullscreen
          update_menu
        end

        def post_iconify
        end

        def post_apply_theme
          recreate_actors
        end

        def post_move(old_index, index)
          actor = @actors[index]
          if actor
            actor.raise_top
            @embed.queue_draw
          end
        end

        def pre_parse
        end

        def post_parse
        end

        def pre_toggle_index_mode
        end

        def post_toggle_index_mode
          recreate_actors
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
          @embed.create_pango_context
        end

        def create_pango_layout(text)
          @embed.create_pango_layout(text)
        end

        def display?
          true
        end

        private
        def init_dpi
          @x_dpi = ScreenInfo.screen_x_resolution
          @y_dpi = ScreenInfo.screen_y_resolution
        end

        def init_clutter_embed
          @embed = Clutter::GtkEmbed.new
          @embed.can_focus = true
          @stage = @embed.stage
          set_map
          set_configure_event_after

          event_mask = Gdk::Event::BUTTON_PRESS_MASK
          event_mask |= Gdk::Event::BUTTON_RELEASE_MASK
          event_mask |= Gdk::Event::BUTTON1_MOTION_MASK
          event_mask |= Gdk::Event::BUTTON2_MOTION_MASK
          event_mask |= Gdk::Event::BUTTON3_MOTION_MASK
          @embed.add_events(event_mask)
          set_key_press_event(@embed)
          set_button_event(@embed)
          set_motion_notify_event
          set_scroll_event
        end

        def depth
          @embed.window.depth
        end

        def draw_nth_slide(i=nil)
          i ||= @canvas.current_index
          slide = @canvas.slides[i]
          if slide
            slide.draw(@canvas, true)
            slide.draw(@canvas, false)
          end
        end

        def set_map
          @embed.signal_connect("map") do |widget, event|
            @drawable = widget.window
            false
          end
        end

        def set_configure_event_after
          @embed.signal_connect_after("configure_event") do |widget, event|
            configured_after(widget, event)
          end
        end

        def configured_after(widget, event)
          recreate_actors
          false
        end

        def set_motion_notify_event
          @embed.signal_connect("motion_notify_event") do |widget, event|
            call_hook_procs(@motion_notify_hook_procs, event)
          end

          begin_x = nil
          begin_y = nil
          prev_x = nil
          prev_y = nil
          start_time = nil
          handled = false
          scale = 1.0
          axis_x = 0.5
          axis_y = 0.5

          add_button_press_hook do |event|
            handled = false
            start_time = event.time
            begin_x = prev_x = event.x
            begin_y = prev_y = event.y
            false
          end

          add_motion_notify_hook do |event|
            state = event.state
            if state.button2_mask?
              handled = true
              processed = true
              x = event.x.to_f
              y = event.y.to_f
              w = width.to_f
              h = height.to_f

              actor = current_actor
              if state.control_mask?
                scale += (y - prev_y) / (h / 4)
                actor.x = (actor.width / 2) * (1 - scale)
                actor.y = (actor.height / 2) * (1 - scale)
                actor.set_scale(scale, scale)
              elsif state.shift_mask?
                angle_x, = actor.get_rotation(Clutter::X_AXIS)
                angle_y, = actor.get_rotation(Clutter::Y_AXIS)
                angle_z, = actor.get_rotation(Clutter::Z_AXIS)

                angle_x += y - prev_y
                angle_y += prev_x - x
                angle_z += (prev_x - x) - (y - prev_y)

                actor.set_rotation(Clutter::X_AXIS, angle_x,
                                   0,
                                   actor.height * axis_y,
                                   0)
                actor.set_rotation(Clutter::Y_AXIS, angle_y,
                                   actor.width * axis_x,
                                   0,
                                   0)
              else
                processed = false
              end

              if processed and event.time > start_time + 100
                start_time = event.time
              end
            end

            prev_x = x
            prev_y = y

            false
          end

          add_button_release_hook do |event, last_button_press_event|
            handled
          end
        end

        def set_scroll_event
          @embed.signal_connect("scroll_event") do |widget, event|
            handled = call_hook_procs(@scroll_hook_procs, event)
            unless handled
              handled = true
              case event.direction
              when Gdk::EventScroll::Direction::UP
                @canvas.activate("PreviousSlide")
              when Gdk::EventScroll::Direction::DOWN
                @canvas.activate("NextSlide")
              else
                handled = false
              end
            end
            handled
          end
        end

        def current_actor
          @actors[@canvas.current_index]
        end

        def recreate_actors
          return if @recreate_id
          i = nil
          @actors = []
          @stage.remove_all
          @recreate_id = GLib::Idle.add do
            index = i || @canvas.current_index
            actor = @actors[index]
            if actor.nil?
              x, y, width, height = @embed.allocation.to_a
              actor = Clutter::Cairo.new(width, height)
              context = actor.create
              init_context(context)
              draw_nth_slide(i)
              finish_context
              @actors[index] = actor
              @stage.add(actor)
              if i.nil?
                redraw
              else
                actor.lower_bottom
              end
              actor.show
            end
            i = -1 if i.nil?
            i += 1
            finish = i < @canvas.slide_size
            if finish
              @recreate_id = nil
              @embed.queue_draw
            end
            finish
          end
        end

        def queue_draw
          @embed.queue_draw
        end

        def grab
          Gtk.grab_add(@embed)
          Gdk.pointer_grab(@embed.window, false,
                           Gdk::Event::BUTTON_PRESS_MASK |
                           Gdk::Event::BUTTON_RELEASE_MASK |
                           Gdk::Event::SCROLL_MASK |
                           Gdk::Event::POINTER_MOTION_MASK,
                           nil, nil,
                           Gdk::Event::CURRENT_TIME)
        end

        def ungrab
          Gtk.grab_remove(@embed)
          Gdk.pointer_ungrab(Gdk::Event::CURRENT_TIME)
        end

        def pointer
          window, x, y, mask = @embed.window.pointer
          [x, y, mask]
        end
      end
    end
  end
end
