proc_name = "headline-logo"

if @headline_logo_image.nil?
  raise "must specify @headline_logo_image!!"
end

match("**", HeadLine) do |heads|

  loader = ImageLoader.new(search_file(@headline_logo_image))

  resized = false
  margin = screen_y(3)

  heads.each do |head|
    start_y = end_y = nil
    
    head.delete_pre_draw_proc_by_name(proc_name)
    head.delete_post_draw_proc_by_name(proc_name)

    head.add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      unless simulation
        unless resized
          header_height = end_y - start_y - margin
          width = ((header_height / loader.height.to_f) * loader.width).ceil
          loader.resize(width, header_height) if width > 0
          resized = true
        end
        canvas.draw_pixbuf(loader.pixbuf, w - loader.width, y)
      end
      start_y = y
      [x, y, w, h]
    end
    
    head.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      end_y = y
      [x, y, w, h]
    end
  end
end
