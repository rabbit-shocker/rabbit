module Rabbit
  module Action
    module_function
    @@radio_blank_values = [
      [:white, 1],
      [:black, 2],
      [:show, 3],
    ]
    def act_radio_blank(action, current, group, canvas)
      case @@radio_blank_values.rassoc(current.value)[0]
      when :white
        canvas.toggle_whiteout
      when :black
        canvas.toggle_blackout
      when :show
        if canvas.whiteouting?
          canvas.toggle_whiteout
        elsif canvas.blackouting?
          canvas.toggle_blackout
        end
      else
        p "????"
      end
    end

    def act_radio_blank_whiteout_config(config, canvas)
      config[:label] = N_("Whiteout")
      config[:value] = @@radio_blank_values.assoc(:white)[1]
      config[:default] = canvas.whiteouting?
    end
    def act_radio_blank_blackout_config(config, canvas)
      config[:label] = N_("Blackout")
      config[:value] = @@radio_blank_values.assoc(:black)[1]
      config[:default] = canvas.blackouting?
    end
    def act_radio_blank_show_config(config, canvas)
      config[:label] = N_("Show")
      config[:value] = @@radio_blank_values.assoc(:show)[1]
      config[:default] = !canvas.whiteouting? && !canvas.blackouting?
    end


    def act_radio_log_level(action, current, group, canvas)
      canvas.logger.level = current.value
    end

    def act_radio_log_level_debug_config(config, canvas)
      config[:label] = N_("Debug")
      config[:value] = Logger::Severity::DEBUG
      config[:default] = canvas.logger.level == config[:value]
    end
    def act_radio_log_level_info_config(config, canvas)
      config[:label] = N_("Info")
      config[:value] = Logger::Severity::INFO
      config[:default] = canvas.logger.level == config[:value]
    end
    def act_radio_log_level_warning_config(config, canvas)
      config[:label] = N_("Warning")
      config[:value] = Logger::Severity::WARNING
      config[:default] = canvas.logger.level == config[:value]
    end
    def act_radio_log_level_error_config(config, canvas)
      config[:label] = N_("Error")
      config[:value] = Logger::Severity::ERROR
      config[:default] = canvas.logger.level == config[:value]
    end
    def act_radio_log_level_fatal_config(config, canvas)
      config[:label] = N_("Fatal")
      config[:value] = Logger::Severity::FATAL
      config[:default] = canvas.logger.level == config[:value]
    end
    def act_radio_log_level_unknown_config(config, canvas)
      config[:label] = N_("Unknown")
      config[:value] = Logger::Severity::UNKNOWN
      config[:default] = canvas.logger.level == config[:value]
    end
  end
end
