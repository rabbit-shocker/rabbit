match(Slide) do |slides|
  slides.margin_left = @margin_left
  slides.margin_right = @margin_right
  slides.margin_top = @margin_top
  slides.margin_bottom = @margin_bottom
end

match(Slide, HeadLine) do |heads|
  name = "head-line"
  
  heads.delete_post_draw_proc_by_name(name)

  space = @space / 2.0
  heads.margin_bottom = space * 3
  heads.add_post_draw_proc(name) do |text, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + w, y + space, "red")
    end
    [x, y, w, h]
  end
end
