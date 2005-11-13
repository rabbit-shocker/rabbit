@category = N_("ToolKit")
@name = N_("PoweredBy")
@abstract = N_("Powered-by ads")
@description = N_("Displays programs the presentation slides are powered " \
                  "by (or displays some other ads) at the foot of the " \
                  "title slide and of the last slide. " \
                  "Images and/or a text are available as ads.")
@parameters = {
  "@powered_by_props" => {
    :default => "{ " \
                "  'size' => screen_size(1.5 * Pango::SCALE), " \
                "  'font_family' => @font_family, " \
                "}",
    :description => N_("Properties of the ad text, such as font family."),
  },
  "@powered_by_images" => {
    :default => "[]",
    :description => N_("Array of ad images."),
  },
  "@powered_by_text" => {
    :default => "nil",
    :description => N_("An ad text."),
  },
}
