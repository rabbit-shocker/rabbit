begin
  include_theme(canvas.title)
rescue LoadError
end

if canvas.allotted_time
  @image_slide_number_image = "mini-usa-taro.png"
  @image_slide_number_show_text = true

  @image_slide_number_margin_left = @margin_left
  @image_slide_number_margin_right = @margin_right
  @image_slide_number_margin_bottom = @margin_bottom
  include_theme("image-slide-number")

  @image_timer_image = "mini-kame-taro.png"
  @image_timer_margin_left = @margin_left
  @image_timer_margin_right = @margin_right
  @image_timer_margin_bottom = @margin_bottom
  include_theme("image-timer")
end

include_theme("default-comment")
