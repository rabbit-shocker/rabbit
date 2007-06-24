include_theme("default-method-list")

match(Slide, Body, MethodList) do
  name = "method-list-indent"

  space = canvas.width * 0.1
  indent(space, name)
end
