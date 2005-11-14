@category = N_("Toolkit")
@name = N_("TitleLogo")
@abstract = N_("Toolkit to display an image as a logo in the title slide")
@description = N_("Displays an image as a logo in the title slide.")
@parameters = {
  "@title_logo_image" => {
    :default => N_("(Must be specified.)"),
    :description => N_("Image file name.")
  },
  "@title_logo_position" => {
    :default => ":right",
    :description => N_("Position of the logo. " \
                       "The logo will be set at the upper-right corner when " \
                       "'(({:right}))', and at the upper-left corner when " \
                       "'(({:left}))'.")
  },
  "@title_logo_image_uninstall" => {
    :default => "nil",
    :description => N_("Whether uninstalling this toolkit or not. " \
                       "This option is useful for cases you do not want to " \
                       "make the toolkit work for certain slides."),
  },
}
