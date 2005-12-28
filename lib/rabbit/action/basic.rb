module Rabbit
  module Action
    module_function
    def act_next(action, group, canvas)
      canvas.move_to_next_if_can
    end
    def act_next_config(config, canvas)
      config[:label] = N_("Next slide")
      config[:stock_id] = Gtk::Stock::GO_FORWARD
    end

    def act_previous(action, group, canvas)
      canvas.move_to_previous_if_can
    end
    def act_previous_config(config, canvas)
      config[:label] = N_("Previous slide")
      config[:stock_id] = Gtk::Stock::GO_BACK
    end

    def act_first(action, group, canvas)
      canvas.move_to_first
    end
    def act_first_config(config, canvas)
      config[:label] = N_("First slide")
      config[:stock_id] = Gtk::Stock::GOTO_FIRST
    end

    def act_last(action, group, canvas)
      canvas.move_to_last
    end
    def act_last_config(config, canvas)
      config[:label] = N_("Last slide")
      config[:stock_id] = Gtk::Stock::GOTO_LAST
    end

    def act_jump(action, group, canvas)
      if action.block_given?
        canvas.move_to_if_can(action.call(action, group, canvas))
      end
    end
    def act_jump_config(config, canvas)
      config[:label] = N_("Jump to")
    end

    def act_save_as_image(action, group, canvas)
      canvas.save_as_image
    end
    def act_save_as_image_config(config, canvas)
      config[:label] = N_("Save as image")
      config[:stock_id] = Gtk::Stock::SAVE
    end

    def act_print(action, group, canvas)
      canvas.print
    end
    def act_print_config(config, canvas)
      config[:label] = N_("Print")
      config[:stock_id] = Gtk::Stock::PRINT
    end

    def act_iconify(action, group, canvas)
      canvas.iconify
    end
    def act_iconify_config(config, canvas)
      config[:label] = N_("Iconify")
      config[:stock_id] = Stock::RABBIT
    end

    def act_change_theme(action, group, canvas)
      if action.block_given?
        canvas.apply_theme(action.call(action, group, canvas).name)
      end
      
    end
    def act_change_theme_config(config, canvas)
      config[:label] = N_("Change theme")
    end

    def act_merge_theme(action, group, canvas)
      if action.block_given?
        canvas.merge_theme(action.call(action, group, canvas).name)
      end
    end
    def act_merge_theme_config(config, canvas)
      config[:label] = N_("Merge theme")
    end

    def act_reload_theme(action, group, canvas)
      canvas.reload_theme
    end
    def act_reload_theme_config(config, canvas)
      config[:label] = N_("Reload theme")
      config[:stock_id] = Gtk::Stock::REFRESH
    end

    def act_redraw(action, group, canvas)
      canvas.redraw
    end
    def act_redraw_config(config, canvas)
      config[:label] = N_("Redraw")
      config[:stock_id] = Gtk::Stock::REFRESH
    end

    def act_quit(action, group, canvas)
      canvas.quit
    end
    def act_quit_config(config, canvas)
      config[:label] = N_("_Quit")
      config[:stock_id] = Gtk::Stock::QUIT
    end

    def act_confirm_quit(action, group, canvas)
      canvas.confirm_quit
    end
    def act_confirm_quit_config(config, canvas)
      config[:label] = N_("Confirm quit")
    end

    def act_cache_all_slides(action, group, canvas)
      canvas.cache_all_slides
    end
    def act_cache_all_slides_config(config, canvas)
      config[:label] = N_("Cache all slides")
    end

    def act_clear_graffiti(action, group, canvas)
      canvas.clear_graffiti
    end
    def act_clear_graffiti_config(config, canvas)
      config[:label] = N_("Clear graffiti")
      config[:stock_id] = Gtk::Stock::CLEAR
    end

    def act_reset_adjustment(action, group, canvas)
      canvas.reset_adjustment
    end
    def act_reset_adjustment_config(config, canvas)
      config[:label] = N_("Reset adjustment")
      config[:stock_id] = Gtk::Stock::CLEAR
    end
  end
end
