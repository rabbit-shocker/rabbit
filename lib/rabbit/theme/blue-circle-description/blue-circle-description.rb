@description_term_line_color = @blue_circle_blue

match(Slide, Body, DescriptionList) do
  name = "description-list-indent"
  space = canvas.width * 0.1
  indent(space, name)
end

include_theme("default-description")
