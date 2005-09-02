proc_name = "title-background-color"

if @title_background_color.nil?
  theme_exit("must specify @title_background_color!!")
end

match(TitleSlide) do |slides|
  slides.delete_pre_draw_proc_by_name(proc_name)

  slides.add_pre_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      args = [0, 0, canvas.width, canvas.height, @title_background_color]
      canvas.draw_rectangle(true, *args)
    end
    [x, y, w, h]
  end
end
