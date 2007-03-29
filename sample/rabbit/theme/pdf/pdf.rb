begin
  include_theme(canvas.title)
rescue LoadError
  @image_slide_number_show_text = true
  @image_slide_number_margin_left = @margin_left
  @image_slide_number_margin_right = @margin_right
  @image_slide_number_margin_bottom = @margin_bottom
  @image_slide_number_image = "mini-usa-taro.png"
  include_theme("image-slide-number")

  @image_timer_margin_left = @margin_left
  @image_timer_margin_right = @margin_right
  @image_timer_margin_bottom = @margin_bottom
  @image_timer_limit = 60
  include_theme("image-timer")
end
