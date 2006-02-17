@title_slide_title_font_size = @large_font_size
include_theme("default-title-text")

match(TitleSlide, "*") do |elems|
  elems.horizontal_centering = false
#  elems.prop_set("weight", "bold")
end
