module Rabbit
  module Action
    module_function
    def act_next_slide(action, group, canvas)
      canvas.move_to_next_if_can
    end
    def act_next_slide_config(config, canvas)
      config[:label] = N_("Next slide")
      config[:stock_id] = Gtk::Stock::GO_FORWARD
    end

    def act_previous_slide(action, group, canvas)
      canvas.move_to_previous_if_can
    end
    def act_previous_slide_config(config, canvas)
      config[:label] = N_("Previous slide")
      config[:stock_id] = Gtk::Stock::GO_BACK
    end

    def act_first_slide(action, group, canvas)
      canvas.move_to_first
    end
    def act_first_slide_config(config, canvas)
      config[:label] = N_("First slide")
      config[:stock_id] = Gtk::Stock::GOTO_FIRST
    end

    def act_last_slide(action, group, canvas)
      canvas.move_to_last
    end
    def act_last_slide_config(config, canvas)
      config[:label] = N_("Last slide")
      config[:stock_id] = Gtk::Stock::GOTO_LAST
    end

    def act_jump_to(action, group, canvas)
      if action.block_given?
        canvas.move_to_if_can(action.call(action, group, canvas))
      end
    end
    def act_jump_to_config(config, canvas)
      config[:label] = N_("Jump to")
      config[:stock_id] = Gtk::Stock::JUMP_TO
    end

    def update_move_slide_action_status(canvas)
      canvas.action("PreviousSlide").sensitive = canvas.have_previous_slide?
      canvas.action("NextSlide").sensitive = canvas.have_next_slide?
      canvas.action("FirstSlide").sensitive = !canvas.first_slide?
      canvas.action("LastSlide").sensitive = !canvas.last_slide?
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
        entry, block = action.call(action, group, canvas)
        canvas.apply_theme(entry.name, &block)
      end
      
    end
    def act_change_theme_config(config, canvas)
      config[:label] = N_("Change theme")
    end

    def act_merge_theme(action, group, canvas)
      if action.block_given?
        entry, block = action.call(action, group, canvas)
        canvas.merge_theme(entry.name, &block)
      end
    end
    def act_merge_theme_config(config, canvas)
      config[:label] = N_("Merge theme")
    end

    def act_reload_theme(action, group, canvas)
      block = nil
      block = action.call(action, group, canvas) if action.block_given?
      canvas.reload_theme(&block)
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

    def act_clear_slide(action, group, canvas)
      canvas.clear_slide
    end
    def act_clear_slide_config(config, canvas)
      config[:label] = N_("Clear slide")
      config[:stock_id] = Gtk::Stock::REFRESH
    end

    def update_theme_action_status(canvas)
      not_applying = !canvas.applying?
      canvas.action("ReloadTheme").sensitive = not_applying
      canvas.action("ChangeTheme").sensitive = not_applying
      canvas.action("MergeTheme").sensitive = not_applying
      canvas.action("CacheAllSlides").sensitive = not_applying
      canvas.action("ToggleFullScreen").sensitive = not_applying
    end

    @@quit_label = N_("_Quit")
    @@quit_with_confirmation_label = N_("_Quit with confirmation")
    def quit_action_label(canvas)
      canvas.processing? ? @@quit_with_confirmation_label : @@quit_label
    end
    
    def act_quit(action, group, canvas)
      if !canvas.processing? or
          canvas.confirm(_("Now processing... Do you really quit?"))
        canvas.quit
      end
    end
    def act_quit_config(config, canvas)
      config[:label] = quit_action_label(canvas)
      config[:stock_id] = Gtk::Stock::QUIT
    end

    def update_quit_action_status(canvas)
      canvas.action("Quit").label = _(quit_action_label(canvas))
    end

    def act_cache_all_slides(action, group, canvas)
      canvas.cache_all_slides
    end
    def act_cache_all_slides_config(config, canvas)
      config[:label] = N_("Cache all slides")
    end

    def act_graffiti(action, group, canvas)
      update_graffiti_action_status(canvas)
    end
    def act_graffiti_config(config, canvas)
      config[:label] = N_("Graffiti")
      config[:stock_id] = Gtk::Stock::EDIT
    end

    def act_clear_graffiti(action, group, canvas)
      canvas.clear_graffiti
    end
    def act_clear_graffiti_config(config, canvas)
      config[:label] = N_("Clear graffiti")
      config[:stock_id] = Gtk::Stock::CLEAR
    end

    def act_undo_graffiti(action, group, canvas)
      canvas.undo_graffiti
    end
    def act_undo_graffiti_config(config, canvas)
      config[:label] = N_("Undo graffiti")
      config[:stock_id] = Gtk::Stock::UNDO
    end

    def act_change_graffiti_color(action, group, canvas)
      canvas.change_graffiti_color
    end
    def act_change_graffiti_color_config(config, canvas)
      config[:label] = N_("Change graffiti color")
      config[:stock_id] = Gtk::Stock::SELECT_COLOR
    end

    def update_graffiti_action_status(canvas)
      graffiti_available = canvas.graffiti_mode? || canvas.have_graffiti?
      canvas.action("Graffiti").sensitive = graffiti_available
      #canvas.action("ClearGraffiti").sensitive = canvas.have_graffiti?
      #canvas.action("UndoGraffiti").sensitive = canvas.can_undo_graffiti?
      canvas.action("ClearGraffiti").sensitive = graffiti_available
      canvas.action("UndoGraffiti").sensitive = graffiti_available
      canvas.action("ChangeGraffitiColor").sensitive = graffiti_available
    end

    def act_reset_adjustment(action, group, canvas)
      canvas.reset_adjustment
    end
    def act_reset_adjustment_config(config, canvas)
      config[:label] = N_("Reset adjustment")
      config[:stock_id] = Gtk::Stock::CLEAR
    end

    def act_expand_hole(action, group, canvas)
      canvas.expand_hole
    end
    def act_expand_hole_config(config, canvas)
      config[:label] = N_("Expand hole")
    end

    def act_narrow_hole(action, group, canvas)
      canvas.narrow_hole
    end
    def act_narrow_hole_config(config, canvas)
      config[:label] = N_("Narrow hole")
    end

    def act_search_slide_forward(action, group, canvas)
      canvas.search_slide(true)
      update_saarch_action_status(canvas)
    end
    def act_search_slide_forward_config(config, canvas)
      config[:label] = N_("Search slide forward")
    end

    def act_search_slide_backward(action, group, canvas)
      canvas.search_slide(false)
      update_saarch_action_status(canvas)
    end
    def act_search_slide_backward_config(config, canvas)
      config[:label] = N_("Search slide backward")
    end

    def act_search_slide_forward_next(action, group, canvas)
      canvas.search_slide(true)
      update_saarch_action_status(canvas)
    end
    def act_search_slide_forward_next_config(config, canvas)
      config[:label] = N_("Search slide forward next")
    end

    def act_search_slide_backward_next(action, group, canvas)
      canvas.search_slide(false)
      update_saarch_action_status(canvas)
    end
    def act_search_slide_backward_next_config(config, canvas)
      config[:label] = N_("Search slide backward next")
    end

    def act_stop_slide_search(action, group, canvas)
      canvas.stop_slide_search
      update_saarch_action_status(canvas)
    end
    def act_stop_slide_search_config(config, canvas)
      config[:label] = N_("Stop slide search")
    end

    def update_saarch_action_status(canvas)
      canvas.action("SearchSlideForward").sensitive = !canvas.searching?
      canvas.action("SearchSlideBackward").sensitive = !canvas.searching?
      canvas.action("SearchSlideForwardNext").sensitive = canvas.searching?
      canvas.action("SearchSlideBackwardNext").sensitive = canvas.searching?
      canvas.action("StopSlideSearch").sensitive = canvas.searching?
    end
  end
end
