@category = N_("Debug")
@name = N_("ShowFrame")
@abstract = N_("Function to show element frames")
@description = N_("Show element frames useful for debug.")
@parameters = {
  "@show_frame_color" => {
    :default => N_("'blue'"),
    :description => N_("Frame color."),
  },
  "@show_frame_uninstall" => {
    :default => N_("nil"),
    :description => N_("Whether uninstalling the function to show frames or " \
                       "not. " \
                       "This option is useful for cases you do not want to " \
                       "show frames for certain slides."),
  },
}
