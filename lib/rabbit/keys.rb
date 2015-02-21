require "gtk3"
require "rabbit/utils"

module Rabbit
  module Keys

    QUIT_KEYS = [
      Gdk::Keyval::KEY_Escape,
      Gdk::Keyval::KEY_q,
    ]

    MOVE_TO_NEXT_KEYS = [
      Gdk::Keyval::KEY_n,
      Gdk::Keyval::KEY_f,
      Gdk::Keyval::KEY_j,
      Gdk::Keyval::KEY_l,
      Gdk::Keyval::KEY_Page_Down,
      Gdk::Keyval::KEY_Tab,
      Gdk::Keyval::KEY_Return,
      Gdk::Keyval::KEY_rightarrow,
      Gdk::Keyval::KEY_downarrow,
      Gdk::Keyval::KEY_space,
      Gdk::Keyval::KEY_plus,
      Gdk::Keyval::KEY_KP_Add,
      Gdk::Keyval::KEY_KP_Page_Down,
      Gdk::Keyval::KEY_KP_Enter,
      Gdk::Keyval::KEY_KP_Tab,
    ]

    MOVE_TO_PREVIOUS_KEYS = [
      Gdk::Keyval::KEY_p,
      Gdk::Keyval::KEY_b,
      Gdk::Keyval::KEY_h,
      Gdk::Keyval::KEY_k,
      Gdk::Keyval::KEY_Page_Up,
      Gdk::Keyval::KEY_leftarrow,
      Gdk::Keyval::KEY_uparrow,
      Gdk::Keyval::KEY_BackSpace,
      Gdk::Keyval::KEY_Delete,
      Gdk::Keyval::KEY_minus,
      Gdk::Keyval::KEY_KP_Subtract,
      Gdk::Keyval::KEY_KP_Page_Up,
      Gdk::Keyval::KEY_KP_Delete,
    ]

    MOVE_TO_FIRST_KEYS = [
      Gdk::Keyval::KEY_a,
      Gdk::Keyval::KEY_Home,
      Gdk::Keyval::KEY_KP_Home,
      Gdk::Keyval::KEY_less,
    ]

    MOVE_TO_LAST_KEYS = [
      Gdk::Keyval::KEY_e,
      Gdk::Keyval::KEY_End,
      Gdk::Keyval::KEY_KP_End,
      Gdk::Keyval::KEY_greater,
      Gdk::Keyval::KEY_dollar,
    ]

    TOGGLE_FULLSCREEN_KEYS = [
      Gdk::Keyval::KEY_F5,
      Gdk::Keyval::KEY_F10,
      Gdk::Keyval::KEY_F11,
    ]

    RELOAD_THEME_KEYS = [
      Gdk::Keyval::KEY_t,
      Gdk::Keyval::KEY_r,
    ]

    SAVE_AS_IMAGE_KEYS = [
      Gdk::Keyval::KEY_s,
    ]

    ICONIFY_KEYS = [
      Gdk::Keyval::KEY_z,
    ]

    TOGGLE_INDEX_MODE_KEYS = [
      Gdk::Keyval::KEY_i,
    ]

    CACHE_ALL_SLIDES_KEYS = [
      Gdk::Keyval::KEY_c,
    ]

    SEARCH_SLIDE_FORWARD_KEYS = [
      Gdk::Keyval::KEY_slash,
    ]

    SEARCH_SLIDE_BACKWARD_KEYS = [
      Gdk::Keyval::KEY_question,
    ]

    SEARCH_SLIDE_FORWARD_NEXT_KEYS = [
      Gdk::Keyval::KEY_n,
    ]

    STOP_SLIDE_SEARCH_KEYS = [
      Gdk::Keyval::KEY_Escape,
    ]

    module Shift
      WHITE_OUT_KEYS = [
        Gdk::Keyval::KEY_w,
      ]

      BLACK_OUT_KEYS = [
        Gdk::Keyval::KEY_b,
      ]

      EXPAND_HOLE_KEYS = [
        Gdk::Keyval::KEY_e,
      ]

      NARROW_HOLE_KEYS = [
        Gdk::Keyval::KEY_n,
      ]

      TOGGLE_GRAFFITI_MODE_KEYS = [
        Gdk::Keyval::KEY_g,
      ]

      SEARCH_SLIDE_BACKWARD_NEXT_KEYS = [
        Gdk::Keyval::KEY_n,
      ]

      TOGGLE_INFO_WINDOW_KEYS = [
        Gdk::Keyval::KEY_i,
      ]
    end

    module Control
      CLEAR_SLIDE_KEYS = [
        Gdk::Keyval::KEY_l,
      ]

      PRINT_KEYS = [
        Gdk::Keyval::KEY_p,
      ]

      UNDO_GRAFFITI_KEYS = [
        Gdk::Keyval::KEY_z,
      ]

      CLEAR_GRAFFITI_KEYS = [
        Gdk::Keyval::KEY_l,
      ]

      SEARCH_SLIDE_FORWARD_KEYS = [
        Gdk::Keyval::KEY_s,
      ]

      SEARCH_SLIDE_BACKWARD_KEYS = [
        Gdk::Keyval::KEY_r,
      ]

      SEARCH_SLIDE_FORWARD_NEXT_KEYS = [
        Gdk::Keyval::KEY_s,
      ]

      SEARCH_SLIDE_BACKWARD_NEXT_KEYS = [
        Gdk::Keyval::KEY_r,
      ]

      STOP_SLIDE_SEARCH_KEYS = [
        Gdk::Keyval::KEY_g,
      ]
    end

    module Alt
      RESET_ADJUSTMENT_KEYS = [
        Gdk::Keyval::KEY_a,
      ]

      RESET_TIMER_KEYS = [
        Gdk::Keyval::KEY_t,
      ]
    end
  end
end
