@default_headline_line_color = "black"
@default_item1_mark_type = "check"
@default_item2_mark_type = "circle"
include_theme("default")

@lightning_talk_proc_name = "lightning-default"
include_theme("lightning-talk-toolkit")

match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.lightning_talk
    end
  end
end
