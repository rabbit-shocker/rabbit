include_theme("default-slide")
include_theme("default-title-text")
include_theme("default-text")
include_theme("image")
include_theme("lightning-talk-toolkit")

match(TitleSlide) do |slides|
  slides.horizontal_centering = true
  slides.vertical_centering = true

  slides.margin_left = @margin_left
  slides.margin_right = @margin_right
  slides.margin_top = @margin_top
  slides.margin_bottom = @margin_bottom
end

match(TitleSlide, "*") do |elems|
  set_font_family(elems)
  elems.prop_set("size", @large_font_size)
end

match(TitleSlide, Title) do |titles|
  set_font_family(titles)
  titles.prop_set("size", @x_large_font_size)
  titles.prop_set("weight", "heavy")

  space = screen_size(5)
  titles.add_post_draw_proc do |title, canvas, x, y, w, h, simulation|
    if title.next_element.is_a?(Subtitle)
      [x, y, w, h]
    else
      [x, y + space, w, h - space]
    end
  end
end

match(TitleSlide, Subtitle) do |titles|
  set_font_family(titles)
  titles.prop_set("size", @normal_font_size)

  space = screen_size(5)
  titles.add_post_draw_proc do |title, canvas, x, y, w, h, simulation|
    if title.next_element.is_a?(Subtitle)
      [x, y, w, h]
    else
      [x, y + space, w, h - space]
    end
  end
end

match(TitleSlide, ContentSource) do |titles|
  set_font_family(titles)
  titles.prop_set("size", @small_font_size)
  titles.prop_set("style", "italic")
end

match(TitleSlide, Institution) do |titles|
  set_font_family(titles)
  titles.prop_set("size", @normal_font_size)
  titles.prop_set("style", "italic")
end

match(Slide) do |slides|
  slides.lightning_talk
end
