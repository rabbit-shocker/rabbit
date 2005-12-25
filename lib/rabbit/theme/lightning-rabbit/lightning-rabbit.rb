include_theme("rabbit")

@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")
props = @lightning_talk_props.dup
props.update(:proc_name => "lightning-rabbit")

match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.lightning_talk(props)
    end
  end
end
