proc_name = "image-slide-number"

@image_slide_number_image ||= "mini-usagi.png"
@image_slide_number_start_image ||= "start-flag.png"
@image_slide_number_end_image ||= "end-flag.png"

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  
  break if @image_slide_number_uninstall
  
  loader = ImageLoader.new(search_file(@image_slide_number_image))
  start_loader = ImageLoader.new(search_file(@image_slide_number_start_image))
  end_loader = ImageLoader.new(search_file(@image_slide_number_end_image))

  initialized = false
  max_width = nil
  start_base_x = nil
  end_base_x = nil
  base_y = nil

  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      unless initialized
        image_height = slide.height / 10
        loader.resize(nil, image_height)
        start_loader.resize(nil, image_height)
        end_loader.resize(nil, image_height)
        max_width = canvas.width - @left_margin - @right_margin
        max_width -= start_loader.width + end_loader.width
        start_base_x = @left_margin
        end_base_x = @left_margin + max_width + start_loader.width
        base_y = canvas.height - @bottom_margin - loader.height
        initialized = true
      end
      ratio = (canvas.current_index - 1.0) / (canvas.slide_size - 1.0)
      base_x = @left_margin + start_loader.width + max_width * ratio
      canvas.draw_pixbuf(start_loader.pixbuf, start_base_x, base_y)
      canvas.draw_pixbuf(loader.pixbuf, base_x, base_y)
      canvas.draw_pixbuf(end_loader.pixbuf, end_base_x, base_y)
    end
    [x, y, w, h]
  end
end
