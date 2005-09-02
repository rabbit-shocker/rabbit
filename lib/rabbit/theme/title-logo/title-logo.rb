proc_name = "title-logo"

if @title_logo_image.nil?
  theme_exit("must specify @title_logo_image!!")
end

match(TitleSlide, Title) do |titles|

  titles.delete_pre_draw_proc_by_name(proc_name)

  break if @title_log_image_uninstall
  
  loader = ImageLoader.new(search_file(@title_logo_image))

  resized = false

  titles.add_pre_draw_proc(proc_name) do |title, canvas, x, y, w, h, simulation|
    unless simulation
      title_slide = title.parent
      unless resized
        title_space = y - title_slide.margin_top
        width = ((title_space / loader.height.to_f) * loader.width).ceil
        loader.resize(width, title_space) if width > 0
        resized = true
      end
      new_x = canvas.width - loader.width - title_slide.margin_right
      new_y = title_slide.margin_top
      canvas.draw_pixbuf(loader.pixbuf, new_x, new_y)
    end
    [x, y, w, h]
  end
end
