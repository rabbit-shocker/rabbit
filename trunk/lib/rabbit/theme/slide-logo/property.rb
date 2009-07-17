@category = N_("Toolkit")
@title = N_("Slide Logo")
@abstract = N_("Toolkit to display an image as logo at the top")
@description = N_("Displays an image as a logo at the top of all slides.")
@parameters = {
  "@slide_logo_image" => {
    :default => N_("(Must be specified.)"),
    :description => N_("Image file name."),
  },
  "@slide_logo_position" => {
    :default => ":right",
    :description => N_("Image position. :right or :left."),
  },
  "@slide_logo_width" => {
    :default => "nil",
    :description => N_("Image width."),
  },
  "@slide_logo_height" => {
    :default => "canvas.height * 0.1",
    :description => N_("Image height."),
  },
  "@slide_logo_uninstall" => {
    :default => "false",
    :description => N_("Whether uninstall this theme or not."),
  },
}
