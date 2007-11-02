match("**", ImageTitleSlide) do |slides|
  slides.vertical_centering = true
end

match("**", Image) do |images|
  images.horizontal_centering = true
end
