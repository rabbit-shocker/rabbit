include_theme("blue-circle-common")

match(Slide) do |slides|
  blue_circle_slide(slides, "slide")
end

match(Slide, HeadLine) do |heads|
  blue_circle_title(heads, 'headline')
end

match(Slide, Body) do |bodies|
  bodies.vertical_centering = true
  bodies.margin_right = canvas.width * 0.05
end
