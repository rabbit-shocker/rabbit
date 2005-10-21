add_theme_path("rabbit-images")

proc_name = "image-slide-number"

@image_slide_number_image ||= "mini-usagi.png"
@image_slide_number_show_text ||= false
@image_slide_number_text_color ||= "white"
@image_slide_number_flag_type ||= "rectangle"
@image_slide_number_start_image ||= "start-flag.png"
@image_slide_number_goal_image ||= "goal-flag.png"

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  
  break if @image_slide_number_uninstall
  
  loader = ImageLoader.new(find_file(@image_slide_number_image))
  unless @image_slide_number_show_text
    start_loader = ImageLoader.new(find_file(@image_slide_number_start_image))
    goal_loader = ImageLoader.new(find_file(@image_slide_number_goal_image))
  end
  
  initialized = false
  max_width = nil
  start_base_x = nil
  goal_base_x = nil
  base_x = nil
  base_y = nil
  max_text_length = Math.log10(canvas.slide_size).truncate + 1
  text_attributes = {
    "size" => @xx_small_font_size / max_text_length,
    "font_family" => @font_family,
    "color" => @image_slide_number_text_color,
    "weight" => "heavy",
  }
  
  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      unless initialized
        height = canvas.height
        image_height =  height / 12
        loader.resize(nil, image_height)

        if @image_slide_number_show_text
          props = {
            "flag_type" => @image_slide_number_flag_type,
            "text" => "%#{max_text_length}d" % canvas.slide_size.to_s,
            "text_attributes" => text_attributes,
          }
          start_flag_width, _ = canvas.flag_size(image_height, props)
          goal_flag_width = start_flag_width
        else
          start_loader.resize(nil, image_height)
          goal_loader.resize(nil, image_height)
          start_flag_width = start_loader.width
          goal_flag_width = goal_loader.width
        end
        
        base_x = @margin_left + start_flag_width
        base_y = height - loader.height - @margin_bottom
        max_width = canvas.width - @margin_left - @margin_right -
          start_flag_width - loader.width
        start_base_x = @margin_left
        goal_base_x = canvas.width - @margin_right - goal_flag_width
        
        initialized = true
      end

      if @image_slide_number_show_text
        props = {
          "flag_type" => @image_slide_number_flag_type,
          "text" => "%0#{max_text_length}d" % canvas.current_index.to_s,
          "text_attributes" => text_attributes,
          "flag_color" => "red",
        }
        canvas.draw_flag(@margin_left, base_y, loader.height, props)

        props["text"] = (canvas.slide_size - 1).to_s
        props["flag_color"] = "blue"
        canvas.draw_flag(goal_base_x, base_y, loader.height, props)
      else
        canvas.draw_pixbuf(start_loader.pixbuf, start_base_x, base_y)
        canvas.draw_pixbuf(goal_loader.pixbuf, goal_base_x, base_y)
      end

      if canvas.slide_size < 3
        ratio = 1
      else
        ratio = (canvas.current_index - 1.0) / (canvas.slide_size - 2.0)
      end
      current_base_x = base_x + max_width * ratio
      canvas.draw_pixbuf(loader.pixbuf, current_base_x, base_y)
    end
    [x, y, w, h]
  end
end
