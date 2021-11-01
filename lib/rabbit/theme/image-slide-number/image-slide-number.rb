add_image_path("rabbit-images")

proc_name = "image-slide-number"

@image_slide_number_image ||= "mini-usa-taro.png"
if !defined?(@image_slide_number_show_text) or
    @image_slide_number_show_text.nil?
  @image_slide_number_show_text = true
end
@image_slide_number_font_size ||= @xx_small_font_size
@image_slide_number_text_color ||= "white"
if !defined?(@image_slide_number_show_flag) or
    @image_slide_number_show_flag.nil?
  @image_slide_number_show_flag = true
end
@image_slide_number_flag_type ||= "rectangle"
@image_slide_number_start_image ||= "start-flag.png"
@image_slide_number_goal_image ||= "goal-flag.png"
@image_slide_number_space_ratio ||= 1.0 / 12.0
@image_slide_number_start_flag_color ||= "red"
@image_slide_number_goal_flag_color ||= "blue"

@image_slide_number_margin_left ||= nil
@image_slide_number_margin_right ||= nil
@image_slide_number_margin_bottom ||= nil

@image_slide_number_draw_parameters ||= {
  :reflect => {:ratio => 0.5, :alpha => 0.5},
  :draw_scaled_pixbuf => false,
}

target_n_slides = nil

match(Slide) do |slides|
  slides.delete_pre_draw_proc_by_name(proc_name)

  break if @image_slide_number_uninstall

  slides.each do |slide|
    if slide["image-slide-number-last-slide"] == "true"
      target_n_slides = slide.index + 1
      break
    end
  end

  loader = ImageLoader.new(find_file(@image_slide_number_image))
  unless @image_slide_number_show_text
    start_loader = ImageLoader.new(find_file(@image_slide_number_start_image))
    goal_loader = ImageLoader.new(find_file(@image_slide_number_goal_image))
  end

  start_flag_width = 0
  goal_flag_width = 0
  if canvas.slide_size < 2
    max_text_length = 1
  else
    max_text_length = Math.log10(canvas.slide_size - 1).truncate + 1
  end
  text_attributes = {
    "size" => @image_slide_number_font_size / max_text_length,
    "font_family" => @font_family,
    "color" => @image_slide_number_text_color,
    "weight" => "heavy",
  }

  slides.add_pre_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    if simulation
      image_height =  canvas.height * @image_slide_number_space_ratio
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
    else
      margin_left = @image_slide_number_margin_left || slide.margin_left
      margin_right = @image_slide_number_margin_right || slide.margin_right
      margin_bottom = @image_slide_number_margin_bottom || slide.margin_bottom
      base_x = margin_left
      base_y = canvas.height - loader.height - margin_bottom
      max_width = canvas.width - margin_right - base_x - loader.width
      start_base_x = base_x
      goal_base_x = canvas.width - margin_right - goal_flag_width

      if @image_slide_number_show_flag
        if @image_slide_number_show_text
          props = {
            "flag_type" => @image_slide_number_flag_type,
            "text" => "%0#{max_text_length}d" % slide.index,
            "text_attributes" => text_attributes,
            "flag_color" => @image_slide_number_start_flag_color,
          }
          canvas.draw_flag(start_base_x, base_y, loader.height, props)

          props["text"] = (canvas.slide_size - 1).to_s
          props["flag_color"] = @image_slide_number_goal_flag_color
          canvas.draw_flag(goal_base_x, base_y, loader.height, props)
        else
          start_loader.draw(canvas, start_base_x, base_y,
                            @image_slide_number_draw_parameters)
          goal_loader.draw(canvas, goal_base_x, base_y,
                           @image_slide_number_draw_parameters)
        end
      end

      target_n_slides ||= canvas.slide_size
      if target_n_slides < 3
        ratio = 1
      else
        ratio = (slide.index - 1.0) / (target_n_slides - 2.0)
        ratio = 1 if ratio > 1
      end
      current_base_x = base_x + max_width * ratio
      loader.draw(canvas, current_base_x, base_y,
                  @image_slide_number_draw_parameters)
    end
    [x, y, w, h]
  end
end
