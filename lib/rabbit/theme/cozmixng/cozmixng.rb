include_theme("rabbit")

@image_with_frame = true
include_theme("image")

@headline_logo_image = "cozmixchu.png"
include_theme("headline-logo")

@powered_by_image = "powered_by_cozmixng_and_rabbit.png"
@powered_by_text = "COZMIXNG and Rabbit"
include_theme("powered-by")

@title_logo_image = "usagi.png"
include_theme("title-logo")

@slide_number_uninstall = true
include_theme("slide-number")

include_theme("image-slide-number")

match(Slide, Body) do |bodies|
  bodies.vertical_centering = true
end
