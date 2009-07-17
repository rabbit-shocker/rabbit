@description_term_line_color ||= @ruby_gnome2_line_color

include_theme("default-description")

match("**", DescriptionTerm) do |terms|
  terms.prop_set("foreground", @ruby_gnome2_color)
end
