proc_name = "title-logo"

if @title_logo_image.nil?
  raise "must specify @title_logo_image!!"
end

match(TitlePage, Title) do |titles|

  loader = ImageLoader.new(search_file(@title_logo_image))

  resized = false

  titles.delete_pre_draw_proc_by_name(proc_name)
  
  titles.add_pre_draw_proc(proc_name) do |title, canvas, x, y, w, h, simulation|
    unless simulation
      title_page = title.parent
      unless resized
        title_space = y - title_page.top_margin
        width = ((title_space / loader.height.to_f) * loader.width).ceil
        loader.resize(width, title_space) if width > 0
        resized = true
      end
      new_x = canvas.width - loader.width - title_page.right_margin
      new_y = title_page.top_margin
      draw_pixbuf(canvas, loader.pixbuf, new_x, new_y)
    end
    [x, y, w, h]
  end
end
