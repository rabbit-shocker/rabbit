@category = N_("Toolkit")
@title = N_("Slide Footer Info")
@abstract = N_("Toolkit to display information at the header")
@description = N_("Displays information with a line at the header of slides.")
@parameters = {
  "@slide_header_info_line_color" => {
    :default => "#666",
    :description => N_("Line color."),
  },
  "@slide_header_info_line_width" => {
    :default => "screen_y(0.1)",
    :description => N_("Line width."),
  },
  "@slide_header_info_line_pattern" => {
    :default => N_("white <-> black gradation"),
    :description => N_("Line fill pattern. @slide_header_info_line_color " \
                       "is overridden by this parameter."),
  },
  "@slide_header_info_text_size" => {
    :default => "screen_size(1.5 * Pango::SCALE)",
    :description => N_("Text size."),
  },
  "@slide_header_info_x_margin" => {
    :default => "screen_x(1)",
    :description => N_("x-axis margin."),
  },
  "@slide_header_info_text_color" => {
    :default => "#666",
    :description => N_("Text color."),
  },
  "@slide_header_info_text_over_line" => {
    :default => "false",
    :description => N_("Whether show a text over line or not."),
  },
  "@slide_header_info_base_y" => {
    :default => "@margin_top",
    :description => N_("Base y-axis position to stroke line."),
  },
  "@slide_header_info_uninstall" => {
    :default => "false",
    :description => N_("Whether uninstall this theme or not."),
  }
}
