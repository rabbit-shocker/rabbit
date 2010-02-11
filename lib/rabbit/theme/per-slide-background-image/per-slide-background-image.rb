match(Slide) do |slides|
  options = {
    :proc_name => "per-slide-background-image",
    :compute_initial_geometry => Proc.new do |canvas, x, y, w, h|
      [0, 0, canvas.width, canvas.height]
    end
  }
  slides.each do |slide|
    apply_background_image_property(slide, options)
  end
end
