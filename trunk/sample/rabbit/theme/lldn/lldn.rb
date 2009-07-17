include_theme("rabbit")

@image_with_frame = false
include_theme("image")

@image_timer_limit = 10 * 60
include_theme("image-timer")

@lightning_talk_as_large_as_possible = true
@lightning_talk_contact_information = "kou@cozmixng.org"
include_theme("lightning-talk-toolkit")
props = {:proc_name => "lightning-rabbit"}

match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.lightning_talk(props)
    end
  end
end
