module Rabbit
  module Action
    module_function
    def act_toggle_white_out(action, group, canvas)
      if canvas.white_outing?
        canvas.activate("RadioBlankShow")
      else
        canvas.activate("RadioBlankWhiteOut")
      end
    end
    def act_toggle_white_out_config(config, canvas)
      config[:label] = N_("White out")
    end

    def act_toggle_black_out(action, group, canvas)
      if canvas.black_outing?
        canvas.activate("RadioBlankShow")
      else
        canvas.activate("RadioBlankBlackOut")
      end
    end
    def act_toggle_black_out_config(config, canvas)
      config[:label] = N_("Black out")
    end

    def act_toggle_comment_frame(action, group, canvas)
      canvas.toggle_comment_frame
    end
    def act_toggle_comment_frame_config(config, canvas)
      config[:label] = N_("Comment frame")
    end

    def act_toggle_comment_view(action, group, canvas)
      canvas.toggle_comment_view
    end
    def act_toggle_comment_view_config(config, canvas)
      config[:label] = N_("Comment view")
    end

    def act_toggle_index_mode(action, group, canvas)
      canvas.toggle_index_mode
    end
    def act_toggle_index_mode_config(config, canvas)
      config[:label] = N_("Index mode")
      config[:stock_id] = Gtk::Stock::INDEX
    end

    def act_toggle_full_screen(action, group, canvas)
      canvas.toggle_fullscreen
    end
    def act_toggle_full_screen_config(config, canvas)
      config[:label] = N_("Full screen")
      if Gtk::Stock.const_defined?(:FULLSCREEN)
        config[:stock_id] = Gtk::Stock::FULLSCREEN
      else
        config[:stock_id] = Gtk::Stock::ZOOM_FIT
      end
    end

    def act_toggle_graffiti_mode(action, group, canvas)
      canvas.toggle_graffiti_mode
    end
    def act_toggle_graffiti_mode_config(config, canvas)
      config[:label] = N_("Graffiti mode")
      config[:stock_id] = Gtk::Stock::EDIT
    end

    def act_toggle_info_window(action, group, canvas)
      canvas.toggle_info_window
    end
    def act_toggle_info_window_config(config, canvas)
      config[:label] = N_("Information window")
    end

    def act_toggle_spotlight(action, group, canvas)
      canvas.toggle_spotlight
    end
    def act_toggle_spotlight_config(config, canvas)
      config[:label] = N_("Spotlight")
    end

    def act_toggle_magnifier(action, group, canvas)
      canvas.toggle_magnifier
    end
    def act_toggle_magnifier_config(config, canvas)
      config[:label] = N_("Magnifier")
    end
  end
end
