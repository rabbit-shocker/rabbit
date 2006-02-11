match("**", DescriptionTerm) do
  name = "description-term"
  
  color = "#ff9900"
  space = @space / 2.0

  margin_with(:bottom => space * 3)
  delete_post_draw_proc_by_name(name)
  add_post_draw_proc(name) do |term, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + term.width, y + space, color)
    end
    [x, y, w, h]
  end
end
