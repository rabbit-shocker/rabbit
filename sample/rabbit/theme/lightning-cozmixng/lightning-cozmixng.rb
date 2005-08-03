include_theme("cozmixng")

@image_timer_limit = 15 * 60
include_theme("image-timer")

include_theme("image-slide-number")

@image_with_frame = false
include_theme("image")

@lightning_talk_contact_information = "http://cozmixng.org/"
@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

proc_name = "lightning-rabbit"

match(Slide, HeadLine) do |heads|
  heads.each do |head|
    slide = head.parent
    if lightning_talk_slide?(slide)
      lightning_talk_slide(slide, proc_name)
      lightning_talk_headline(head, proc_name)
    end
  end
end
