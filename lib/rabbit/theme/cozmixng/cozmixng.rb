include_theme("rabbit")

add_theme_path("cozmixng-images")

set_foreground("black")
set_background("white")

if @image_with_frame.nil?
  @image_with_frame = true
end
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


match(Slide, HeadLine) do |heads|
  heads.horizontal_centering = false
end

match(Slide, Body) do |bodies|
  bodies.vertical_centering = true
end


@lightning_talk_contact_information ||= "http://cozmixng.org/"
@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

proc_name = "cozmixng"

match(Slide, HeadLine) do |heads|
  heads.each do |head|
    slide = head.parent
    if lightning_talk_slide?(slide)
      lightning_talk_slide(slide, proc_name)
      lightning_talk_headline(head, proc_name)
    end
  end
end
