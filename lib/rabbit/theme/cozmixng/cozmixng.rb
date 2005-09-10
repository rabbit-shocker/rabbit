include_theme("rabbit")

@image_with_frame = true
include_theme("image")

@headline_logo_image = "cozmixchu.png"
include_theme("headline-logo")

include_theme("cozmixng-powered-by")

@title_logo_image = "usagi.png"
include_theme("title-logo")

@slide_number_uninstall = true
include_theme("slide-number")

include_theme("image-slide-number")

@icon_images = ["cozmixchu.png"]
include_theme("icon")


match(Slide, Body) do |bodies|
#  bodies.vertical_centering = true
end
