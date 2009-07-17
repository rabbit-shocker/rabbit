match("**", MethodDescription) do
  name = "method-description"

  space = @normal_font_size / Pango::SCALE
  indent(space, name)
end
