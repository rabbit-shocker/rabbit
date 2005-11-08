@category = N_("Time")
@name = N_("TextClock")
@abstract = N_("Clock, a text version")
@description = N_("Display current time with text.")
@parameters = {
  "@clock_auto_update" => {
    :default => "true",
    :description => "Whether update clock automatically or not.",
  },
  "@clock_props" => {
    :default => "{'size' => @xx_small_font_size, " \
                "'font_family' => @font_family}",
    :description => N_("Properties for clocks, such as font family."),
  },
  "@clock_uninstall" => {
    :default => "nil",
    :description => N_("Whether uninstall clock or not."),
  },
}
