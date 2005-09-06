match(TitleSlide, "*") do |elems|
  elems.prop_set("size", @large_font_size)
  set_font_family(elems)
end

match(TitleSlide, Title) do |titles|
  titles.prop_set("size", @huge_font_size)
  titles.prop_set("weight", "heavy")
end

match(TitleSlide, Subtitle) do |titles|
  titles.prop_set("size", @normal_font_size)
end

match(TitleSlide, ContentSource) do |sources|
  sources.prop_set("size", @small_font_size)
  sources.prop_set("style", "italic")

  sources.margin_bottom = @space
end

match(TitleSlide, Institution) do |institutions|
  institutions.prop_set("size", @normal_font_size)
  institutions.prop_set("style", "italic")
end

match(TitleSlide, "**", SmallText) do |texts|
  texts.prop_set("size", @small_font_size)
end
