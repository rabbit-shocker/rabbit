include_theme("blue-circle-common")

match(TitleSlide) do |slides|
  blue_circle_slide(slides, "title-slide")
end

match(TitleSlide, Title) do |titles|
  blue_circle_title(titles, 'title')
end

match(TitleSlide, Subtitle) do |titles|
  titles.margin_left = canvas.width * 0.2
  titles.margin_right = canvas.width * 0.05
end
