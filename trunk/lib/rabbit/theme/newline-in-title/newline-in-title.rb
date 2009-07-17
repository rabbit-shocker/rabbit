match(TitleSlide, "*") do |elements|
  elements.substitute_newline
end

match(Slide, HeadLine) do |headlines|
  headlines.substitute_newline
end
