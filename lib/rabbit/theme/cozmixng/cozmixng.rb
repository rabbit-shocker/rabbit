include_theme("rabbit")

@headline_logo_image = "cozmixchu.png"

include_theme("headline-logo")

@powered_by_image = "powered_by_cozmixng_and_rabbit.png"
@powered_by_text = "COZMIXNG and Rabbit"

include_theme("powered-by")

@title_logo_image = "usagi.png"

include_theme("title-logo")

match(Slide, Body) do |bodies|
  bodies.vertical_centering = true
end
