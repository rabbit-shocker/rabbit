@preformatted_frame_color = "#3399cc"
@preformatted_fill_color = "#ddeeff"
include_theme("default-preformatted")

match("**", PreformattedText) do |texts|
  texts.prop_set("size", @x_small_font_size)
  set_font_family(texts, @monospace_font_family)
end
