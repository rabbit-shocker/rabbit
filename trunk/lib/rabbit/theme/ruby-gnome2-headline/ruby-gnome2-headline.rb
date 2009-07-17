match(Slide, HeadLine) do |heads|
  name = "head-line"
  
  heads.delete_post_draw_proc_by_name(name)

  heads.prop_set("foreground", "#ffffff")
  heads.horizontal_centering = true

  x_space = screen_x(2)
  y_space = screen_y(1)

  heads.padding_top = y_space * 2
  heads.padding_bottom = y_space * 2

  border_color = "#eecccc"
  back_color = "#7e1000"
  
  heads.each do |head|
    pre_y = nil
    width = nil
    height = nil
    head.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
      pre_y = y
      unless simulation
        border_x = x
        border_y = y - y_space * 2
        border_width = width
        border_height = height + y_space * 4
        canvas.draw_rectangle(true, border_x, border_y,
                              border_width, border_height, border_color)

        back_x = x + x_space
        back_y = y - y_space
        back_width = width - x_space * 2
        back_height = height + y_space * 2
        canvas.draw_rectangle(true, back_x, back_y,
                              back_width, back_height, back_color)
      end
      [x, y, w, h]
    end
    head.add_post_draw_proc(name) do |canvas, x, y, w, h, simulation|
      width = w
      height = y - pre_y
      [x, y, w, h]
    end
  end
end
