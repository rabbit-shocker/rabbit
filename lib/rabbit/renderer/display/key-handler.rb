require 'gtk2'

require "rabbit/keys"

module Rabbit
  module Renderer
    module Display
      module KeyHandler
        def initialize(*args, &block)
          super
          init_key_handler
        end

        def connect_key(keyval, modifier, flags, &block)
          @user_accel_group.connect(keyval, modifier, flags, &block)
        end

        def disconnect_key(keyval, modifier)
          @user_accel_group.disconnect_key(keyval, modifier)
        end

        private
        def init_key_handler
          @user_accel_group = nil
          init_accel_group
        end

        def clear_user_accel_group
          @user_accel_group = Gtk::AccelGroup.new
        end

        def attach_key(window)
          window.add_accel_group(@accel_group)
        end

        def detach_key(window)
          window.remove_accel_group(@accel_group)
        end

        def clear_keys
          super
          @window.remove_accel_group(@user_accel_group) if @user_accel_group
          clear_user_accel_group
          @window.add_accel_group(@user_accel_group)
        end

        def init_accel_group
          @accel_group = Gtk::AccelGroup.new
          init_number_keys
          init_no_prefix_keys
          init_shift_keys
          init_control_keys
          init_alt_keys
        end

        def set_keys(keys, mod, flags=nil, &block)
          flags ||= Gtk::AccelFlags::VISIBLE
          keys.each do |val|
            @accel_group.connect(val, mod, flags, &block)
          end
        end

        def calc_slide_number(val, modifier)
          val += 10 if modifier.control_mask?
          val += 20 if modifier.mod1_mask?
          val
        end

        def init_number_keys
          no_mod = Gdk::Window::ModifierType.new
          mods = Utils.combination([
                                    Gdk::Window::ModifierType::CONTROL_MASK,
                                    Gdk::Window::ModifierType::MOD1_MASK,
                                   ])
          mods.each do |mod|
            mod = mod.inject(no_mod) do |result, item|
              result | item
            end
            keys = (0..9).collect{|i| Gdk::Keyval.const_get("GDK_KEY_#{i}")}
            set_keys(keys, mod) do |group, obj, val, modifier|
              index = calc_slide_number(val - Gdk::Keyval::GDK_KEY_0, modifier)
              @canvas.activate("JumpTo") {index}
            end
            keys = (0..9).collect{|i| Gdk::Keyval.const_get("GDK_KEY_KP_#{i}")}
            set_keys(keys, mod) do |group, obj, val, modifier|
              index = calc_slide_number(val - Gdk::Keyval::GDK_KEY_KP_0, modifier)
              @canvas.activate("JumpTo") {index}
            end
          end
        end

        def init_no_prefix_keys
          mod = Gdk::Window::ModifierType.new

          keys = Keys::QUIT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("Quit")
          end
          keys = Keys::MOVE_TO_NEXT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("Next")
          end
          keys = Keys::MOVE_TO_PREVIOUS_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("Previous")
          end
          keys = Keys::MOVE_TO_FIRST_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("FirstSlide")
          end
          keys = Keys::MOVE_TO_LAST_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("LastSlide")
          end
          keys = Keys::TOGGLE_FULLSCREEN_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ToggleFullScreen")
          end
          keys = Keys::RELOAD_THEME_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            reload_theme
          end
          keys = Keys::SAVE_AS_IMAGE_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SaveAsImage")
          end
          keys = Keys::ICONIFY_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("Iconify")
          end
          keys = Keys::TOGGLE_INDEX_MODE_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ToggleIndexMode")
          end
          keys = Keys::CACHE_ALL_SLIDES_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("CacheAllSlides")
          end
          keys = Keys::SEARCH_SLIDE_FORWARD_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SearchSlideForward")
          end
          keys = Keys::SEARCH_SLIDE_BACKWARD_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SearchSlideBackward")
          end
          keys = Keys::SEARCH_SLIDE_FORWARD_NEXT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SearchSlideForwardNext")
          end
          keys = Keys::STOP_SLIDE_SEARCH_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("StopSlideSearch")
          end
        end

        def init_shift_keys
          mod = Gdk::Window::SHIFT_MASK

          keys = Keys::Shift::WHITE_OUT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ToggleWhiteout")
          end
          keys = Keys::Shift::BLACK_OUT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ToggleBlackout")
          end
          keys = Keys::Shift::EXPAND_HOLE_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ExpandHole")
          end
          keys = Keys::Shift::NARROW_HOLE_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("NarrowHole")
          end
          keys = Keys::Shift::TOGGLE_GRAFFITI_MODE_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ToggleGraffitiMode")
          end
          keys = Keys::Shift::SEARCH_SLIDE_BACKWARD_NEXT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SearchSlideBackwardNext")
          end
          keys = Keys::Shift::TOGGLE_INFO_WINDOW_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ToggleInfoWindow")
          end
        end

        def init_control_keys
          mod = Gdk::Window::CONTROL_MASK

          keys = Keys::Control::CLEAR_GRAFFITI_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ClearGraffiti")
          end
          keys = Keys::Control::UNDO_GRAFFITI_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("UndoGraffiti")
          end

          keys = Keys::Control::CLEAR_SLIDE_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ClearSlide")
          end
          keys = Keys::Control::PRINT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("Print")
          end
          keys = Keys::Control::SEARCH_SLIDE_FORWARD_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SearchSlideForward")
          end
          keys = Keys::Control::SEARCH_SLIDE_BACKWARD_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SearchSlideBackward")
          end
          keys = Keys::Control::SEARCH_SLIDE_FORWARD_NEXT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SearchSlideForwardNext")
          end
          keys = Keys::Control::SEARCH_SLIDE_BACKWARD_NEXT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("SearchSlideBackwardNext")
          end
          keys = Keys::Control::STOP_SLIDE_SEARCH_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("StopSlideSearch")
          end

          keys = Keys::MOVE_TO_NEXT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("NextSlide")
          end
          keys = Keys::MOVE_TO_PREVIOUS_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("PreviousSlide")
          end
        end

        def init_alt_keys
          mod = Gdk::Window::MOD1_MASK

          keys = Keys::Alt::RESET_ADJUSTMENT_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ResetAdjustment")
          end

          keys = Keys::Alt::RESET_TIMER_KEYS
          set_keys(keys, mod) do |group, obj, val, modifier|
            @canvas.activate("ResetTimer")
          end
        end

        def set_key_press_event(widget)
          prev_keys = [
            Gdk::Keyval::GDK_KEY_Up,
            Gdk::Keyval::GDK_KEY_Left,
            Gdk::Keyval::GDK_KEY_KP_Up,
            Gdk::Keyval::GDK_KEY_KP_Left,
          ]
          next_keys = [
            Gdk::Keyval::GDK_KEY_Right,
            Gdk::Keyval::GDK_KEY_Down,
            Gdk::Keyval::GDK_KEY_KP_Right,
            Gdk::Keyval::GDK_KEY_KP_Down,
          ]
          widget.signal_connect("key_press_event") do |_widget, event|
            handled = true
            modifier = event.state
            case event.keyval
            when *prev_keys
              if modifier.nonzero?
                index = calc_slide_number(0, modifier)
                @canvas.activate("JumpTo") {@canvas.current_index - index}
              else
                @canvas.activate("PreviousSlide")
              end
            when *next_keys
              if modifier.nonzero?
                index = calc_slide_number(0, modifier)
                @canvas.activate("JumpTo") {@canvas.current_index + index}
              else
                @canvas.activate("NextSlide")
              end
            else
              handled = false
            end
            handled
          end
        end
      end
    end
  end
end
