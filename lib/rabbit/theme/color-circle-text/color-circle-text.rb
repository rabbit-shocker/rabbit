include_theme("default-text")

match(Slide, "*") do |elems|
#  elems.prop_set("weight", "bold")
end

match(Slide, Body, Paragraph) do |texts|
  texts.margin_left = canvas.width * 0.10
end
