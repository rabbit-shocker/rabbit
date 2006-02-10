match(Slide) do
  margin_set(@margin_top, @margin_right, @margin_bottom, @margin_left)
end

match(Slide, HeadLine) do
  name = "head-line"
  
  delete_post_draw_proc_by_name(name)

  space = @space / 2.0
  margin_set(:bottom => space * 3)
  add_post_draw_proc(name) do |text, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + w, y + space, "red")
    end
    [x, y, w, h]
  end
end
