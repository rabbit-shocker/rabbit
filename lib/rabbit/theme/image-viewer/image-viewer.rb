include_theme("default-comment")

match(ImageTitleSlide) do |slides|
  slides.vertical_centering = true
end

match("**", Image) do |images|
  images.horizontal_centering = true
end

if canvas.allotted_time
  margin_left   = @margin_left
  margin_right  = @margin_right
  margin_bottom = @margin_bottom

  @image_timer_margin_left   = margin_left
  @image_timer_margin_right  = margin_right
  @image_timer_margin_bottom = margin_bottom
  @image_timer_target_paths  = [ImageTitleSlide]
  include_theme("image-timer")
end
