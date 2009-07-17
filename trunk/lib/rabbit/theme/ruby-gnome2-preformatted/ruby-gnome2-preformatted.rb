@preformatted_frame_color = @ruby_gnome2_frame_color
@preformatted_fill_color = @ruby_gnome2_fill_color
include_theme("default-preformatted")

match("**", PreformattedText) do |texts|
  texts.prop_set("size", @x_small_font_size)
  set_font_family(texts, @monospace_font_family)
end
