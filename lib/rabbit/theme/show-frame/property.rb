@category = N_("Debug")
@name = N_("ShowFrame")
@abstract = N_("Debug toolkit to show element frames")
@description = N_("Show element frames useful for debug.")
@parameters = {
  "@show_frame_color" => {
    :default => "'blue'",
    :description => N_("Frame color."),
  },
  "@show_frame_uninstall" => {
    :default => "nil",
    :description => N_("Whether uninstalling this toolkit or not. " \
                       "This option is useful for cases you do not want to " \
                       "make the toolkit work for certain slides."),
  },
}
