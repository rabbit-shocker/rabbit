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

require_relative "gtk"
require_relative "keys"

module Rabbit
  class KeyHandler
    def initialize(canvas, window)
      @canvas = canvas
      @window = window
      init
    end

    if Gtk.const_defined?(:ShortcutController)
      def connect_key(keyval, modifier, flags, &block)
        trigger = Gtk::KeyvalTrigger.new(keyval, modifier)
        action = Gio::SimpleAction.new
        action.signal_connect do
          block.call
        end
        shortcut = Gtk::Shortcut.new(trigger, action)
        @user_shortcuts.append(shortcut)
      end

      def disconnect_key(keyval, modifier)
        @user_shortcuts.each_with_index do |shortcut, i |
          if shortcut.keyval == keyval and shortcut.modifier == modifier
            @user_shortcuts.remove(i)
            break
          end
        end
      end

      def pre_terminal
        @window.remove_controller(@user_shortcut_controller)
        @window.remove_controller(@shortcut_controller)
      end

      def post_terminal
        @window.add_controller(@shortcut_controller)
        @window.add_controller(@user_shortcut_controller)
      end

      def clear
        super
        clear_user_shortcuts
      end

      def detach
        @window.insert_action_group("rabbit", nil)
        @window.remove_controller(@user_shortcut_controller)
        @window.remove_controller(@toggle_terminal_shortcut_controller)
        @window.remove_controller(@shortcut_controller)
      end

      private
      def init
        @user_shortcuts = Gio::ListStore.new(Gtk::Shortcut)
        @user_shortcut_controller =
          Gtk::ShortcutController.new(@user_shortcuts)
        init_shortcuts
        init_toggle_terminal_shortcuts
        attach
      end

      def clear_user_shortcuts
        @user_shortcuts.remove_all
      end

      def attach
        @window.insert_action_group("rabbit", @canvas.actions.group)
        @window.add_controller(@shortcut_controller)
        @window.add_controller(@toggle_terminal_shortcut_controller)
        @window.add_controller(@user_shortcut_controller)
      end

      def init_shortcuts
        @shortcuts = Gio::ListStore.new(Gtk::Shortcut)
        @shortcut_controller = Gtk::ShortcutController.new(@shortcuts)
        init_keys
      end

      def init_toggle_terminal_shortcuts
        @toggle_terminal_shortcuts = Gio::ListStore.new(Gtk::Shortcut)
        @toggle_terminal_shortcut_controller =
          Gtk::ShortcutController.new(@toggle_terminal_shortcuts)
        keys = Keys::ShiftControlAlt::TOGGLE_TERMINAL_KEYS
        modifiers = Gdk::ModifierType::SHIFT_MASK |
                    Gdk::ModifierType::CONTROL_MASK |
                    Gdk::ModifierType::ALT_MASK
        set_keys("ToggleTerminal",
                 nil,
                 keys,
                 modifiers,
                 @toggle_terminal_shortcuts)
      end

      def set_keys(action_name,
                   action_arguments,
                   keys,
                   modifiers,
                   shortcuts=@shortcuts)
        keys.each do |key|
          trigger = Gtk::KeyvalTrigger.new(key, modifiers)
          action = Gtk::NamedAction.new("rabbit.#{action_name}")
          shortcut = Gtk::Shortcut.new(trigger, action)
          shortcut.arguments = action_arguments
          shortcuts.append(shortcut)
        end
      end
    else
      def connect_key(keyval, modifier, flags, &block)
        @user_accel_group.connect(keyval, modifier, flags, &block)
      end

      def disconnect_key(keyval, modifier)
        @user_accel_group.disconnect_key(keyval, modifier)
      end

      def pre_terminal
        @window.remove_accel_group(@accel_group)
      end

      def post_terminal
        @window.add_accel_group(@accel_group)
      end

      def detach
        @window.signal_handler_disconnect(@key_press_event_id)
        @key_press_event_id = nil
        @window.remove_accel_group(@accel_group)
        @window.remove_accel_group(@toggle_terminal_accel_group)
      end

      def clear
        @window.remove_accel_group(@user_accel_group) if @user_accel_group
        clear_user_accel_group
        @window.add_accel_group(@user_accel_group)
      end

      private
      def init
        @user_accel_group = nil
        init_accel_group
        init_toggle_terminal_accel_group
        attach
      end

      def clear_user_accel_group
        @user_accel_group = Gtk::AccelGroup.new
      end

      def attach
        @window.add_accel_group(@accel_group)
        @window.add_accel_group(@toggle_terminal_accel_group)
        set_key_press_event
      end

      def init_accel_group
        @accel_group = Gtk::AccelGroup.new
        init_keys
      end

      def init_toggle_terminal_accel_group
        @toggle_terminal_accel_group = Gtk::AccelGroup.new
        mod = Gdk::ModifierType::SHIFT_MASK |
              Gdk::ModifierType::CONTROL_MASK |
              Gdk::ModifierType::ALT_MASK
        keys = Keys::ShiftControlAlt::TOGGLE_TERMINAL_KEYS
        set_keys("ToggleTerminal",
                 nil,
                 keys,
                 mod,
                 nil,
                 @toggle_terminal_accel_group)
      end

      def set_keys(action_name,
                   action_arguments,
                   keys,
                   mod,
                   flags=nil,
                   accel_group=@accel_group)
        flags ||= Gtk::AccelFlags::VISIBLE
        keys.each do |val|
          accel_group.connect(val, mod, flags) do |group, obj, val, modifier|
            if action_arguments
              @canvas.activate(action_name, action_arguments)
            else
              @canvas.activate(action_name)
            end
          end
        end
      end

      def set_key_press_event
        # We can't use Gtk::AccelGroup with unmodified direction
        # keys. See gtk_accelerator_valid() for details.
        prev_keys = [
          Gdk::Keyval::KEY_Up,
          Gdk::Keyval::KEY_Left,
          Gdk::Keyval::KEY_KP_Up,
          Gdk::Keyval::KEY_KP_Left,
        ]
        next_keys = [
          Gdk::Keyval::KEY_Right,
          Gdk::Keyval::KEY_Down,
          Gdk::Keyval::KEY_KP_Right,
          Gdk::Keyval::KEY_KP_Down,
        ]
        @key_press_event_id = @window.signal_connect("key_press_event") do |_widget, event|
          handled = true
          modifier = event.state
          case event.keyval
          when *prev_keys
            if have_slide_number_related_mask?(modifier)
              index = calc_slide_number(0, modifier)
              @canvas.activate("JumpToRelativeSlide",
                               GLib::Variant.new(-index, "i"))
            else
              @canvas.activate("PreviousSlide")
            end
          when *next_keys
            if have_slide_number_related_mask?(modifier)
              index = calc_slide_number(0, modifier)
              @canvas.activate("JumpToRelativeSlide",
                               GLib::Variant.new(index, "i"))
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

    def init_keys
      init_move_slide_keys
      init_no_prefix_keys
      init_shift_keys
      init_control_keys
      init_alt_keys
    end

    def have_slide_number_related_mask?(modifier)
      modifier.control_mask? or
        modifier.alt_mask?
    end

    def calc_slide_number(val, modifier)
      val += 10 if modifier.control_mask?
      val += 20 if modifier.alt_mask?
      val
    end

    def init_move_slide_keys
      no_mod = Gdk::ModifierType.new
      mods = Utils.power_set([
                               Gdk::ModifierType::CONTROL_MASK,
                               Gdk::ModifierType::ALT_MASK,
                             ])
      mods.each do |mod|
        mod = mod.inject(no_mod) do |result, item|
          result | item
        end
        (0..9).each do |i|
          key = Gdk::Keyval.const_get("KEY_#{i}")
          index = calc_slide_number(i, mod)
          set_keys("JumpToSlide", GLib::Variant.new(index, "i"), [key], mod)
        end
        (0..9).each do |i|
          key = Gdk::Keyval.const_get("KEY_KP_#{i}")
          index = calc_slide_number(i, mod)
          set_keys("JumpToSlide", GLib::Variant.new(index, "i"), [key], mod)
        end
        if mod == no_mod
          set_keys("NextSlide",
                   nil,
                   Keys::MOVE_TO_NEXT_SLIDE_KEYS,
                   mod)
          set_keys("PreviousSlide",
                   nil,
                   Keys::MOVE_TO_PREVIOUS_SLIDE_KEYS,
                   mod)
        else
          set_keys("JumpToRelativeSlide",
                   GLib::Variant.new(calc_slide_number(0, mod), "i"),
                   Keys::MOVE_TO_NEXT_SLIDE_KEYS,
                   mod)
          set_keys("JumpToRelativeSlide",
                   GLib::Variant.new(-calc_slide_number(0, mod), "i"),
                   Keys::MOVE_TO_PREVIOUS_SLIDE_KEYS,
                   mod)
        end
      end
    end

    def init_no_prefix_keys
      mod = Gdk::ModifierType.new
      [
        ["Quit", Keys::QUIT_KEYS],
        ["Next", Keys::MOVE_TO_NEXT_KEYS],
        ["Previous", Keys::MOVE_TO_PREVIOUS_KEYS],
        ["FirstSlide", Keys::MOVE_TO_FIRST_KEYS],
        ["LastSlide", Keys::MOVE_TO_LAST_KEYS],
        ["ToggleFullscreen", Keys::TOGGLE_FULLSCREEN_KEYS],
        ["ReloadTheme", Keys::RELOAD_THEME_KEYS],
        ["SaveAsImage", Keys::SAVE_AS_IMAGE_KEYS],
        ["Iconify", Keys::ICONIFY_KEYS],
        ["ToggleIndexMode", Keys::TOGGLE_INDEX_MODE_KEYS],
        ["CacheAllSlides", Keys::CACHE_ALL_SLIDES_KEYS],
        ["SearchSlideForward", Keys::SEARCH_SLIDE_FORWARD_KEYS],
        ["SearchSlideBackward", Keys::SEARCH_SLIDE_BACKWARD_KEYS],
        ["SearchSlideForwardNext", Keys::SEARCH_SLIDE_FORWARD_NEXT_KEYS],
      ].each do |action_name, keys|
        set_keys(action_name, nil, keys, mod)
      end
    end

    def init_shift_keys
      mod = Gdk::ModifierType::SHIFT_MASK
      [
        ["ToggleWhiteout", Keys::Shift::WHITE_OUT_KEYS],
        ["ToggleBlackout", Keys::Shift::BLACK_OUT_KEYS],
        ["ExpandHole", Keys::Shift::EXPAND_HOLE_KEYS],
        ["NarrowHole", Keys::Shift::NARROW_HOLE_KEYS],
        ["ToggleGraffitiMode", Keys::Shift::TOGGLE_GRAFFITI_MODE_KEYS],
        ["SearchSlideBackwardNext",
         Keys::Shift::SEARCH_SLIDE_BACKWARD_NEXT_KEYS],
        ["ToggleInfoWindow", Keys::Shift::TOGGLE_INFO_WINDOW_KEYS],
      ].each do |action_name, keys|
        set_keys(action_name, nil, keys, mod)
      end
    end

    def init_control_keys
      mod = Gdk::ModifierType::CONTROL_MASK
      [
        ["ClearGraffiti", Keys::Control::CLEAR_GRAFFITI_KEYS],
        ["UndoGraffiti", Keys::Control::UNDO_GRAFFITI_KEYS],
        ["ClearSlide", Keys::Control::CLEAR_SLIDE_KEYS],
        ["Print", Keys::Control::PRINT_KEYS],
        ["SearchSlideForward", Keys::Control::SEARCH_SLIDE_FORWARD_KEYS],
        ["SearchSlideBackward", Keys::Control::SEARCH_SLIDE_BACKWARD_KEYS],
        ["SearchSlideForwardNext",
         Keys::Control::SEARCH_SLIDE_FORWARD_NEXT_KEYS],
        ["SearchSlideBackwardNext",
         Keys::Control::SEARCH_SLIDE_BACKWARD_NEXT_KEYS],
        ["StopSlideSearch", Keys::Control::STOP_SLIDE_SEARCH_KEYS],
      ].each do |action_name, keys|
        set_keys(action_name, nil, keys, mod)
      end
    end

    def init_alt_keys
      mod = Gdk::ModifierType::ALT_MASK
      [
        ["ResetAdjustment", Keys::Alt::RESET_ADJUSTMENT_KEYS],
        ["ResetTimer", Keys::Alt::RESET_TIMER_KEYS],
      ].each do |action_name, keys|
        set_keys(action_name, nil, keys, mod)
      end
    end
  end
end
