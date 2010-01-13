proc_name = "per-slide-background-image"

match(Slide) do |slides|
  slides.each do |slide|
    slide.delete_pre_draw_proc_by_name(proc_name)

    background_image = slide["background-image"]
    next if background_image.nil?

    properties = {}
    slide.user_property.each do |name, value|
      if /\Abackground[_-]image[_-]/ =~ name
        properties[$POSTMATCH.gsub(/-/, '_')] = value
      end
    end
    image = Image.new(canvas.full_path(background_image), properties)
    image.horizontal_centering = true
    slide.add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      if simulation
        image.compile(canvas, x, y, w, h)
        if image.do_horizontal_centering?
          image.do_horizontal_centering(canvas, x, y, w, h)
        end
      end
      image.draw(simulation)
      [x, y, w, h]
    end
  end
end
