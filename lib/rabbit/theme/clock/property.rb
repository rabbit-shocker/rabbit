@category = N_("Time")
@name = N_("TextClock")
@abstract = N_("Clock, a text version")
@description = N_("Displays current time with text.")
@parameters = {
  "@clock_auto_update" => {
    :default => "true",
    :description => "Whether updating the clock automatically or not.",
  },
  "@clock_props" => {
    :default => "{'size' => @xx_small_font_size, " \
                "'font_family' => @font_family}",
    :description => N_("Properties for the clock, such as font family."),
  },
  "@clock_uninstall" => {
    :default => "nil",
    :description => N_("Whether uninstalling the function to display the " \
                       "clock or not. " \
                       "This option is useful for cases you do not want to " \
                       "display clock for certain slides."),
  },
}
