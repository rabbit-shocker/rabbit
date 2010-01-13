proc_name = "per-slide-background-color"

match(Slide) do |slides|
  slides.each do |slide|
    slide.delete_pre_draw_proc_by_name(proc_name)

    background_color = slide["background-color"]
    next if background_color.nil?

    slide.add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      unless simulation
        canvas.draw_rectangle(true, 0, 0, canvas.width, canvas.height,
                              background_color)
      end
      [x, y, w, h]
    end
  end
end
