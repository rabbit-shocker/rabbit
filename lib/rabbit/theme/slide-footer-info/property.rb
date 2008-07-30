@category = N_("Toolkit")
@title = N_("Slide Footer Info")
@abstract = N_("Toolkit to display information at the footer")
@description = N_("Displays information with a bar at the footer of slides.")
@parameters = {
  "@slide_footer_info_line_color" => {
    :default => "#666",
    :description => N_("Bar color."),
  },
  "@slide_footer_info_line_width" => {
    :default => "screen_y(0.1)",
    :description => N_("Bar width."),
  },
  "@slide_footer_info_line_pattern" => {
    :default => N_("white <-> black gradation"),
    :description => N_("Bar fill pattern. @slide_footer_info_line_color "
                       "is overridden by this parameter."),
  },
  "@slide_footer_info_text_size" => {
    :default => "screen_size(1.5 * Pango::SCALE)",
    :description => N_("Text size."),
  },
  "@slide_footer_info_x_margin" => {
    :default => "screen_x(1)",
    :description => N_("x-axis margin."),
  },
  "@slide_footer_info_text_color" => {
    :default => "#666",
    :description => N_("Text color."),
  },
  "@slide_footer_info_text_over_line" => {
    :default => "false",
    :description => N_("Whether show a text over line or not."),
  },
  "@slide_footer_info_base_y" => {
    :default => "canvas.height - @margin_bottom",
    :description => N_("Base y-axis position to stroke bar."),
  },
  "@slide_footer_info_uninstall" => {
    :default => "false",
    :description => N_("Whether uninstall this theme or not."),
  }
}
