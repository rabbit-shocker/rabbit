@description_term_line_color = @color_circle_color

match(Slide, Body, DescriptionList) do
  name = "description-list-indent"
  space = canvas.width * 0.1
  indent(space, name)
end

include_theme("default-description")
