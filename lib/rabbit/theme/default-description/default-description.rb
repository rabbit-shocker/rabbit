match("**", DescriptionTerm) do |terms|
  name = "description-term"
  
  color = "#ff9900"
  space = @space / 2.0

  terms.margin_bottom = space * 3
  terms.delete_post_draw_proc_by_name(name)
  terms.add_post_draw_proc(name) do |term, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + term.width, y + space, color)
    end
    [x, y, w, h]
  end
end
