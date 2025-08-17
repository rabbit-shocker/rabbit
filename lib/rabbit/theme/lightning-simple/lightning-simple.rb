include_theme("image")
include_theme("video")
include_theme("table")

normal_font_size = @normal_font_size
small_font_size = @small_font_size
xx_small_font_size = @xx_small_font_size
@normal_font_size = screen_size(4 * Pango::SCALE)
@small_font_size = screen_size(3.3 * Pango::SCALE)
@xx_small_font_size = screen_size(2.5 * Pango::SCALE)
include_theme("default-text")
@normal_font_size = normal_font_size
@small_font_size = small_font_size
@xx_small_font_size = xx_small_font_size

x_large_font_size = @x_large_font_size
large_font_size = @large_font_size
normal_font_size = @normal_font_size
@x_large_font_size = screen_size(7 * Pango::SCALE)
@large_font_size = screen_size(4.5 * Pango::SCALE)
@normal_font_size = screen_size(3.5 * Pango::SCALE)
include_theme("default-title-text")
@x_large_font_size = x_large_font_size
@large_font_size = large_font_size
@normal_font_size = normal_font_size

include_theme("default-title-slide")
include_theme("default-slide")
include_theme("default-description")
include_theme("default-preformatted")

match("**", PreformattedText) do |texts|
  texts.prop_set("size", screen_size(2.8 * Pango::SCALE))
end

include_theme("rabbit-item-mark")

@powered_by_text = "Powered by Rabbit #{VERSION}"
include_theme("powered-by")

if print?
  include_theme("slide-number")
else
  include_theme("image-slide-number")

  @image_timer_limit ||= canvas.allotted_time || 5 * 60
  include_theme("image-timer")
end

@lightning_talk_proc_name = "lightning-simple"
@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.lightning_talk
    end
  end
end
