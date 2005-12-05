add_image_path("ruby-images")
add_image_path("rabbit-images")

include_theme("image")
include_theme("table")
include_theme("default-text")

set_background("white")

@title_logo_image = "ruby-pink-polygon.png"
@title_logo_position = :left
include_theme("title-logo")

include_theme("ruby-gnome2-icon")
include_theme("ruby-gnome2-slide")
include_theme("ruby-gnome2-headline")
include_theme("ruby-gnome2-item-mark")
include_theme("ruby-gnome2-preformatted")
include_theme("ruby-gnome2-description")

match("**", Emphasis) do |ems|
  ems.prop_set("foreground", "#006600")
end

@image_slide_number_image = "mini-usa-taro.png"
@image_slide_number_show_text = true
include_theme("image-slide-number")

if print?
  @powered_by_text = "Powered by Rabbit #{VERSION}, Ruby-GNOME2 and COZMIXNG"
end
@powered_by_images = [
   "rabbit-banner-green.png",
   "rabbit-banner-blue.png",
   "rabbit-banner-pink.png",
]
include_theme("powered-by")

@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

proc_name = "lightning-ruby-gnome2"

match(Slide, HeadLine) do |heads|
  heads.each do |head|
    slide = head.parent
    if lightning_talk_slide?(slide)
      lightning_talk_slide(slide, proc_name)
      lightning_talk_headline(head, proc_name)
      head.prop_set("foreground", "black")
    end
  end
end

include_theme("windows-adjust")
