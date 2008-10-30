include_theme("pdf")

@image_slide_number_image = "mini-usa-taro.png"
@image_slide_number_show_text = true
include_theme("image-slide-number")

canvas.title_slide['allotted-time'] = ENV["RABBIT_ALLOTTED_TIME"]
if canvas.allotted_time
  @image_timer_image = "mini-kame-taro.png"
  include_theme("image-timer")
end
