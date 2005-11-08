include_theme("rabbit")

add_theme_path("cozmixng-images")

@image_with_frame = true
include_theme("image")

@headline_logo_image = "cozmixchu.png"
include_theme("headline-logo")

include_theme("cozmixng-powered-by")

@title_logo_image = "ruby-pink-polygon-logo.png"
include_theme("title-logo")

@slide_number_uninstall = true
include_theme("slide-number")

@image_slide_number_image = "mini-usa-taro.png"
@image_slide_number_show_text = true
include_theme("image-slide-number")
if !print? and canvas.title_slide.allotted_time
  @image_timer_image = "mini-kame-taro.png"
  include_theme("image-timer")
end

@icon_images = ["cozmixchu.png"]
include_theme("icon")


match(Slide, Body) do |bodies|
  bodies.vertical_centering = true
end
