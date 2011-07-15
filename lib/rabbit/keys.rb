require "gtk2"
require "rabbit/utils"

# For GTK+ < 2.22
unless Gdk::Keyval.const_defined?(:GDK_KEY_Escape)
  Gdk::Keyval.constants.each do |name|
    Gdk::Keyval.const_set(name.to_s.sub(/\AGDK_/, "GDK_KEY_"),
                          Gdk::Keyval.const_get(name))
  end
end

module Rabbit
  module Keys

    QUIT_KEYS = [
      Gdk::Keyval::GDK_KEY_Escape,
      Gdk::Keyval::GDK_KEY_q,
    ]

    MOVE_TO_NEXT_KEYS = [
      Gdk::Keyval::GDK_KEY_n,
      Gdk::Keyval::GDK_KEY_f,
      Gdk::Keyval::GDK_KEY_j,
      Gdk::Keyval::GDK_KEY_l,
      Gdk::Keyval::GDK_KEY_Page_Down,
      Gdk::Keyval::GDK_KEY_Tab,
      Gdk::Keyval::GDK_KEY_Return,
      Gdk::Keyval::GDK_KEY_rightarrow,
      Gdk::Keyval::GDK_KEY_downarrow,
      Gdk::Keyval::GDK_KEY_space,
      Gdk::Keyval::GDK_KEY_plus,
      Gdk::Keyval::GDK_KEY_KP_Add,
      Gdk::Keyval::GDK_KEY_KP_Page_Down,
      Gdk::Keyval::GDK_KEY_KP_Enter,
      Gdk::Keyval::GDK_KEY_KP_Tab,
    ]

    MOVE_TO_PREVIOUS_KEYS = [
      Gdk::Keyval::GDK_KEY_p,
      Gdk::Keyval::GDK_KEY_b,
      Gdk::Keyval::GDK_KEY_h,
      Gdk::Keyval::GDK_KEY_k,
      Gdk::Keyval::GDK_KEY_Page_Up,
      Gdk::Keyval::GDK_KEY_leftarrow,
      Gdk::Keyval::GDK_KEY_uparrow,
      Gdk::Keyval::GDK_KEY_BackSpace,
      Gdk::Keyval::GDK_KEY_Delete,
      Gdk::Keyval::GDK_KEY_minus,
      Gdk::Keyval::GDK_KEY_KP_Subtract,
      Gdk::Keyval::GDK_KEY_KP_Page_Up,
      Gdk::Keyval::GDK_KEY_KP_Delete,
    ]

    MOVE_TO_FIRST_KEYS = [
      Gdk::Keyval::GDK_KEY_a,
      Gdk::Keyval::GDK_KEY_Home,
      Gdk::Keyval::GDK_KEY_KP_Home,
      Gdk::Keyval::GDK_KEY_less,
    ]

    MOVE_TO_LAST_KEYS = [
      Gdk::Keyval::GDK_KEY_e,
      Gdk::Keyval::GDK_KEY_End,
      Gdk::Keyval::GDK_KEY_KP_End,
      Gdk::Keyval::GDK_KEY_greater,
      Gdk::Keyval::GDK_KEY_dollar,
    ]

    TOGGLE_FULLSCREEN_KEYS = [
      Gdk::Keyval::GDK_KEY_F5,
      Gdk::Keyval::GDK_KEY_F10,
      Gdk::Keyval::GDK_KEY_F11,
    ]

    RELOAD_THEME_KEYS = [
      Gdk::Keyval::GDK_KEY_t,
      Gdk::Keyval::GDK_KEY_r,
    ]

    SAVE_AS_IMAGE_KEYS = [
      Gdk::Keyval::GDK_KEY_s,
    ]

    ICONIFY_KEYS = [
      Gdk::Keyval::GDK_KEY_z,
    ]

    TOGGLE_INDEX_MODE_KEYS = [
      Gdk::Keyval::GDK_KEY_i,
    ]

    CACHE_ALL_SLIDES_KEYS = [
      Gdk::Keyval::GDK_KEY_c,
    ]

    SEARCH_SLIDE_FORWARD_KEYS = [
      Gdk::Keyval::GDK_KEY_slash,
    ]

    SEARCH_SLIDE_BACKWARD_KEYS = [
      Gdk::Keyval::GDK_KEY_question,
    ]

    SEARCH_SLIDE_FORWARD_NEXT_KEYS = [
      Gdk::Keyval::GDK_KEY_n,
    ]

    STOP_SLIDE_SEARCH_KEYS = [
      Gdk::Keyval::GDK_KEY_Escape,
    ]

    module Shift
      WHITE_OUT_KEYS = [
        Gdk::Keyval::GDK_KEY_w,
      ]

      BLACK_OUT_KEYS = [
        Gdk::Keyval::GDK_KEY_b,
      ]

      EXPAND_HOLE_KEYS = [
        Gdk::Keyval::GDK_KEY_e,
      ]

      NARROW_HOLE_KEYS = [
        Gdk::Keyval::GDK_KEY_n,
      ]

      TOGGLE_GRAFFITI_MODE_KEYS = [
        Gdk::Keyval::GDK_KEY_g,
      ]

      SEARCH_SLIDE_BACKWARD_NEXT_KEYS = [
        Gdk::Keyval::GDK_KEY_n,
      ]

      TOGGLE_INFO_WINDOW_KEYS = [
        Gdk::Keyval::GDK_KEY_i,
      ]
    end

    module Control
      CLEAR_SLIDE_KEYS = [
        Gdk::Keyval::GDK_KEY_l,
      ]

      PRINT_KEYS = [
        Gdk::Keyval::GDK_KEY_p,
      ]

      UNDO_GRAFFITI_KEYS = [
        Gdk::Keyval::GDK_KEY_z,
      ]

      CLEAR_GRAFFITI_KEYS = [
        Gdk::Keyval::GDK_KEY_l,
      ]

      SEARCH_SLIDE_FORWARD_KEYS = [
        Gdk::Keyval::GDK_KEY_s,
      ]

      SEARCH_SLIDE_BACKWARD_KEYS = [
        Gdk::Keyval::GDK_KEY_r,
      ]

      SEARCH_SLIDE_FORWARD_NEXT_KEYS = [
        Gdk::Keyval::GDK_KEY_s,
      ]

      SEARCH_SLIDE_BACKWARD_NEXT_KEYS = [
        Gdk::Keyval::GDK_KEY_r,
      ]

      STOP_SLIDE_SEARCH_KEYS = [
        Gdk::Keyval::GDK_KEY_g,
      ]
    end

    module Alt
      RESET_ADJUSTMENT_KEYS = [
        Gdk::Keyval::GDK_KEY_a,
      ]

      RESET_TIMER_KEYS = [
        Gdk::Keyval::GDK_KEY_t,
      ]
    end
  end
end
