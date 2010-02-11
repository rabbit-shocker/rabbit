match(Slide, Body) do |bodies|
  options = {
    :proc_name => "body-background-image",
  }
  bodies.each do |body|
    apply_background_image_property(body, options)
  end
end
