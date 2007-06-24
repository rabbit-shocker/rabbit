include_theme("default")

add_image_path("rabbit-images")

set_foreground("black")
if print? and slides_per_page > 1
  set_background("white")
else
  set_background("#f500f1d0c600")
end
# set_background_image("lavie.png")

## orange(?) base
# set_progress_foreground("#ff00da00d200")
# set_progress_background("#fd05f3f3fa0b")

## pink base
set_progress_foreground("#ff00cc00ff00")
set_progress_background("#fffff3f3f711")

## green base
# set_progress_foreground("#eb29f6f6e535")
# set_progress_background("#eb29f6f6df41")

set_graffiti_color("red")

if @image_with_frame.nil?
  @image_with_frame = true
end
include_theme("image")

include_theme("rabbit-title-logo")
include_theme("rabbit-headline-logo")

@title_shadow_color = "#c09090cc"
include_theme("title-shadow")

@slide_number_uninstall = true
include_theme("slide-number")

@image_slide_number_show_text = true
include_theme("image-slide-number")

include_theme("rabbit-powered-by")
include_theme("rabbit-icon")

match(TitleSlide, Title) do |titles|
  titles.prop_set("foreground", "red")
  titles.prop_set("style", "italic")
end

match(Slide, HeadLine) do |heads|
  heads.horizontal_centering = true
end

slide_body = [Slide, Body]

# match(*slide_body) do |bodies|
#   bodies.vertical_centering = true
# end

include_theme("rabbit-item-mark")
include_theme("rabbit-block-quote")

include_theme("windows-adjust")
