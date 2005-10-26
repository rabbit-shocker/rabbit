include_theme("default-title-slide")
include_theme("default-slide")

sidebar = Proc.new do |slide, canvas, x, y, w, h, simulation|
  unless simulation
    line_x = canvas.width * 0.2
    canvas.draw_line(line_x, 0, line_x, canvas.height, "red")
    canvas.draw_rectangle(true, 0, 0, line_x, canvas.height, "#fff1ec")
  end
  [x, y, w, h]
end

match(TitleSlide) do |slides|
  name = "title-slide"
  
  slides.delete_post_draw_proc_by_name(name)
  
  slides.add_pre_draw_proc(name, &sidebar)
end

match(TitleSlide, Title) do |titles|
  titles.prop_set("foreground", "red")
end

@title_shadow_color = "#c09090"
include_theme("title-shadow")

match(Slide) do |slides|
  name = "slide"
  
  slides.delete_post_draw_proc_by_name(name)
  
  slides.add_pre_draw_proc(name, &sidebar)
end

match(Slide, Body) do |bodies|
  padding = @space * 2
  bodies.padding_left = padding
  bodies.padding_right = padding
end
