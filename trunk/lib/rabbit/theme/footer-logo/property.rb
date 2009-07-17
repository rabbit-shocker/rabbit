@category = N_("Toolkit")
@title = N_("Footer Logo")
@abstract = N_("Toolkit to display an image as logo at the footer")
@description = N_("Displays an image as a logo at the footer of slides.")
@parameters = {
  "@footer_logo_image" => {
    :default => N_("(Must be specified.)"),
    :description => N_("Image file name."),
  },
  "@footer_logo_keep_ratio" => {
    :default => "true",
    :description => N_("Whether keep ratio of an image or not."),
  },
  "@footer_logo_margin_right" => {
    :default => "@margin_right",
    :description => N_("Right margin of an image."),
  },
  "@footer_logo_margin_bottom" => {
    :default => "@margin_bottom + screen_y(1)",
    :description => N_("Bottom margin of an image."),
  },
  "@footer_logo_uninstall" => {
    :default => "false",
    :description => N_("Whether uninstall this theme or not."),
  },
}
