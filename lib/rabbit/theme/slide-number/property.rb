@category = N_("Toolkit")
@name = N_("TextSlideNumber")
@abstract = N_("Toolkit to display slide numbers, a text version")
@description = N_("Displays slide numbers with text at the bottom of the " \
                  "slides.")
@parameters = {
  "@slide_number_props" => {
    :default => "{ " \
                "  'size' => @xx_small_font_size, " \
                "  'font_family' => @font_family, " \
                "}",
    :description => N_("Properties of the slide numbers, such as font " \
                       "family."),
  },
  "@slide_number_uninstall" => {
    :default => "nil",
    :description => N_("Whether uninstalling this toolkit or not. " \
                       "This option is useful for cases you do not want to " \
                       "make the toolkit work for certain slides."),
  },
}
