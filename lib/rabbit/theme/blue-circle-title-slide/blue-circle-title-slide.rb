include_theme("blue-circle-common")

match(TitleSlide) do |slides|
  blue_circle_slide(slides, "title-slide")
end

match(TitleSlide, "*") do |elements|
  elements.each do |element|
    element.margin_left = canvas.width * 0.2
    element.margin_right = canvas.width * 0.05
    unless [Title, Subtitle, Author].find {|type| element.is_a?(type)}
      element.align = Pango::Layout::ALIGN_RIGHT
    end
  end
end

match(TitleSlide, Title) do |titles|
  blue_circle_title(titles, 'title')
end

match(TitleSlide, Author) do |authors|
  authors.margin_top = @space * 3
end

match(TitleSlide, Institution) do |institutions|
  institutions.margin_top = @space
end
