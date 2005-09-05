match(TitleSlide) do |slides|
  slides.horizontal_centering = true
  slides.vertical_centering = true

  slides.margin_left = @margin_left
  slides.margin_right = @margin_right
  slides.margin_top = @margin_top
  slides.margin_bottom = @margin_bottom
end

match(TitleSlide, Author) do |authors|
  authors.margin_top = @space * 2
  authors.margin_bottom = @space
end
