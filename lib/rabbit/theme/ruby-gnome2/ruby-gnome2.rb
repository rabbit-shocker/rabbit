add_image_path("ruby-images")
add_image_path("rabbit-images")

include_theme("image")
include_theme("table")
include_theme("default-title-text")
include_theme("default-text")
include_theme("default-method-list")

set_background("white")

set_graffiti_color("red")

@title_logo_image = "ruby-pink-polygon.png"
@title_logo_position = :left
include_theme("title-logo")

@ruby_gnome2_color ||= "#003399"
@ruby_gnome2_emphasis_color ||= "#006600"
@ruby_gnome2_line_color ||= "#ff9999"
@ruby_gnome2_frame_color ||= "#3399cc"
@ruby_gnome2_fill_color ||= "#ddeeff"

include_theme("ruby-gnome2-icon")
include_theme("ruby-gnome2-slide")
include_theme("ruby-gnome2-headline")
include_theme("ruby-gnome2-item-mark")
include_theme("ruby-gnome2-preformatted")
include_theme("ruby-gnome2-description")
include_theme("ruby-gnome2-foot-text")

match("**", Emphasis) do |ems|
  ems.prop_set("foreground", @ruby_gnome2_emphasis_color)
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

@lightning_talk_proc_name = "lightning-ruby-gnome2"
@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.lightning_talk
    end
  end
end

include_theme("windows-adjust")
