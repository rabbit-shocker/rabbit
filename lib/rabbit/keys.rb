require "gtk2"

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
      Gdk::Keyval::GDK_Right,
      Gdk::Keyval::GDK_Down,
      Gdk::Keyval::GDK_KP_Add,
      Gdk::Keyval::GDK_KP_Right,
      Gdk::Keyval::GDK_KP_Down,
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
      Gdk::Keyval::GDK_Up,
      Gdk::Keyval::GDK_Left,
      Gdk::Keyval::GDK_KP_Subtract,
      Gdk::Keyval::GDK_KP_Up,
      Gdk::Keyval::GDK_KP_Left,
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
    
    WHITE_OUT_KEYS = [
      Gdk::Keyval::GDK_W,
    ]

    BLACK_OUT_KEYS = [
      Gdk::Keyval::GDK_B,
    ]
    
    TOGGLE_COMMENT_FRAME_KEYS = [
      Gdk::Keyval::GDK_m,
    ]
    
    TOGGLE_COMMENT_VIEW_KEYS = [
      Gdk::Keyval::GDK_M,
    ]

    EXPAND_HOLE_KEYS = [
      Gdk::Keyval::GDK_E,
    ]
    
    NARROW_HOLE_KEYS = [
      Gdk::Keyval::GDK_N,
    ]
    
    module Control
      REDRAW_KEYS = [
        Gdk::Keyval::GDK_l,
      ]
      
      PRINT_KEYS = [
        Gdk::Keyval::GDK_p,
      ]
    end

    module Alt
      RESET_ADJUSTMENT_KEYS = [
        Gdk::Keyval::GDK_a,
      ]
    end
  end
end
