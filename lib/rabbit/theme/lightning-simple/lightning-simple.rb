include_theme("image")
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

huge_font_size = @huge_font_size
large_font_size = @large_font_size
normal_font_size = @normal_font_size
@huge_font_size = screen_size(7 * Pango::SCALE)
@large_font_size = screen_size(4.5 * Pango::SCALE)
@normal_font_size = screen_size(3.5 * Pango::SCALE)
include_theme("default-title-text")
@huge_font_size = huge_font_size
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

@powered_by_text = "Rabbit #{VERSION}"
include_theme("powered-by")

if print?
  include_theme("slide-number")
else
  @image_slide_number_show_text = true
  include_theme("image-slide-number")

  @image_timer_limit ||= 5 * 60
  include_theme("image-timer")
end

@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

proc_name = "lightning-simple"

match(Slide, HeadLine) do |heads|
  heads.each do |head|
    slide = head.parent
    if lightning_talk_slide?(slide)
      lightning_talk_slide(slide, proc_name)
      lightning_talk_headline(head, proc_name)
    end
  end
end
