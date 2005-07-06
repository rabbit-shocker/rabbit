include_theme("rabbit")

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
