match(Slide) do
  margin_set(@margin_top, @margin_right, @margin_bottom, @margin_left)
end

match(Slide, HeadLine) do
  name = "head-line"
  
  delete_post_draw_proc_by_name(name)

  space = @space / 2.0
  margin_with(:bottom => space * 3)
  add_post_draw_proc(name) do |text, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + w, y + space, "#ff9933")
    end
    [x, y, w, h]
  end
end

match(Slide, Body) do |bodies|
  bodies.each do |body|
    unless body.elements.any? {|element| element.is_a?(Image)}
      body.margin_with(:left => @body_margin_left,
                       :right => @body_margin_right)
    end
  end
end
