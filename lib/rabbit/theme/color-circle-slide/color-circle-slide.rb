match(Slide) do |slides|
  color_circle_slide(slides)
end

match(Slide, HeadLine) do |heads|
  color_circle_title(heads)
end

match(Slide, Body) do |bodies|
  bodies.vertical_centering = true
  bodies.margin_left = canvas.width * 0.05
  bodies.margin_right = canvas.width * 0.05
end
