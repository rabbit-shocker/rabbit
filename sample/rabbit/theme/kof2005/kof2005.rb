@huge_font_size = (@huge_font_size * 0.8).ceil

include_theme("ruby-gnome2")

unless print?
  @image_timer_image = "mini-kame-taro.png"
  @image_timer_limit ||= 15 * 60
  include_theme("image-timer")
end
