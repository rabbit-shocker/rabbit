module Rabbit
  module Action
    module_function
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
      config[:stock_id] = Gtk::Stock::FULLSCREEN
    end

    def act_toggle_graffiti_mode(action, group, canvas)
      canvas.toggle_graffiti_mode
    end
    def act_toggle_graffiti_mode_config(config, canvas)
      config[:label] = N_("Graffiti mode")
      config[:stock_id] = Gtk::Stock::EDIT
    end
  end
end
