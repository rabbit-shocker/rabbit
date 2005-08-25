include_theme("rabbit")

@image_with_frame = false
include_theme("image")

@image_timer_limit = 10 * 60
include_theme("image-timer")

@lightning_talk_as_large_as_possible = true
@lightning_talk_contact_information = "http://192.168.5.1:10102/"
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
