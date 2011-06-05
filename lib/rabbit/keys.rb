require "gtk2"
require "rabbit/utils"

module Rabbit
  module Keys

    QUIT_KEYS = [
      Gdk::Keyval::GDK_Escape,
      Gdk::Keyval::GDK_q,
    ]

    MOVE_TO_NEXT_KEYS = [
      Gdk::Keyval::GDK_n,
      Gdk::Keyval::GDK_f,
      Gdk::Keyval::GDK_j,
      Gdk::Keyval::GDK_l,
      Gdk::Keyval::GDK_Page_Down,
      Gdk::Keyval::GDK_Tab,
      Gdk::Keyval::GDK_Return,
      Gdk::Keyval::GDK_rightarrow,
      Gdk::Keyval::GDK_downarrow,
      Gdk::Keyval::GDK_space,
      Gdk::Keyval::GDK_plus,
      Gdk::Keyval::GDK_KP_Add,
      Gdk::Keyval::GDK_KP_Page_Down,
      Gdk::Keyval::GDK_KP_Enter,
      Gdk::Keyval::GDK_KP_Tab,
    ]

    MOVE_TO_PREVIOUS_KEYS = [
      Gdk::Keyval::GDK_p,
      Gdk::Keyval::GDK_b,
      Gdk::Keyval::GDK_h,
      Gdk::Keyval::GDK_k,
      Gdk::Keyval::GDK_Page_Up,
      Gdk::Keyval::GDK_leftarrow,
      Gdk::Keyval::GDK_uparrow,
      Gdk::Keyval::GDK_BackSpace,
      Gdk::Keyval::GDK_Delete,
      Gdk::Keyval::GDK_minus,
      Gdk::Keyval::GDK_KP_Subtract,
      Gdk::Keyval::GDK_KP_Page_Up,
      Gdk::Keyval::GDK_KP_Delete,
    ]

    MOVE_TO_FIRST_KEYS = [
      Gdk::Keyval::GDK_a,
      Gdk::Keyval::GDK_Home,
      Gdk::Keyval::GDK_KP_Home,
      Gdk::Keyval::GDK_less,
    ]

    MOVE_TO_LAST_KEYS = [
      Gdk::Keyval::GDK_e,
      Gdk::Keyval::GDK_End,
      Gdk::Keyval::GDK_KP_End,
      Gdk::Keyval::GDK_greater,
      Gdk::Keyval::GDK_dollar,
    ]

    TOGGLE_FULLSCREEN_KEYS = [
      Gdk::Keyval::GDK_F5,
      Gdk::Keyval::GDK_F10,
      Gdk::Keyval::GDK_F11,
    ]

    RELOAD_THEME_KEYS = [
      Gdk::Keyval::GDK_t,
      Gdk::Keyval::GDK_r,
    ]

    SAVE_AS_IMAGE_KEYS = [
      Gdk::Keyval::GDK_s,
    ]

    ICONIFY_KEYS = [
      Gdk::Keyval::GDK_z,
    ]

    TOGGLE_INDEX_MODE_KEYS = [
      Gdk::Keyval::GDK_i,
    ]

    CACHE_ALL_SLIDES_KEYS = [
      Gdk::Keyval::GDK_c,
    ]

    SEARCH_SLIDE_FORWARD_KEYS = [
      Gdk::Keyval::GDK_slash,
    ]

    SEARCH_SLIDE_BACKWARD_KEYS = [
      Gdk::Keyval::GDK_question,
    ]

    SEARCH_SLIDE_FORWARD_NEXT_KEYS = [
      Gdk::Keyval::GDK_n,
    ]

    STOP_SLIDE_SEARCH_KEYS = [
      Gdk::Keyval::GDK_Escape,
    ]

    module Shift
      WHITE_OUT_KEYS = [
        Gdk::Keyval::GDK_w,
      ]

      BLACK_OUT_KEYS = [
        Gdk::Keyval::GDK_b,
      ]

      EXPAND_HOLE_KEYS = [
        Gdk::Keyval::GDK_e,
      ]

      NARROW_HOLE_KEYS = [
        Gdk::Keyval::GDK_n,
      ]

      TOGGLE_GRAFFITI_MODE_KEYS = [
        Gdk::Keyval::GDK_g,
      ]

      SEARCH_SLIDE_BACKWARD_NEXT_KEYS = [
        Gdk::Keyval::GDK_n,
      ]

      TOGGLE_INFO_WINDOW_KEYS = [
        Gdk::Keyval::GDK_i,
      ]
    end

    module Control
      CLEAR_SLIDE_KEYS = [
        Gdk::Keyval::GDK_l,
      ]

      PRINT_KEYS = [
        Gdk::Keyval::GDK_p,
      ]

      UNDO_GRAFFITI_KEYS = [
        Gdk::Keyval::GDK_z,
      ]

      CLEAR_GRAFFITI_KEYS = [
        Gdk::Keyval::GDK_l,
      ]

      SEARCH_SLIDE_FORWARD_KEYS = [
        Gdk::Keyval::GDK_s,
      ]

      SEARCH_SLIDE_BACKWARD_KEYS = [
        Gdk::Keyval::GDK_r,
      ]

      SEARCH_SLIDE_FORWARD_NEXT_KEYS = [
        Gdk::Keyval::GDK_s,
      ]

      SEARCH_SLIDE_BACKWARD_NEXT_KEYS = [
        Gdk::Keyval::GDK_r,
      ]

      STOP_SLIDE_SEARCH_KEYS = [
        Gdk::Keyval::GDK_g,
      ]
    end

    module Alt
      RESET_ADJUSTMENT_KEYS = [
        Gdk::Keyval::GDK_a,
      ]

      RESET_TIMER_KEYS = [
        Gdk::Keyval::GDK_t,
      ]
    end
  end
end
