match("**", Image) do |images|
  images.horizontal_centering = true

  space = screen_size(3)

  make_normalized_size = Proc.new do |size|
    size && screen_size(size)
  end

  make_relative_size = Proc.new do |size, parent_size|
    size && parent_size && ((size / 100.0) * parent_size).ceil
  end

  images.each do |image|
    image.add_pre_draw_proc do |canvas, x, y, w, h, simulation|
      nw = make_normalized_size.call(image.normalized_width)
      nh = make_normalized_size.call(image.normalized_height)
      rw = make_relative_size.call(image.relative_width, w)
      rh = make_relative_size.call(image.relative_height, h)
      iw = nw || rw
      ih = nh || rh
      image.resize(iw, ih)
      [x, y, w, h]
    end
  end

  images.each do |image|
    layout = nil
    th = 0
    image.add_post_draw_proc do |canvas, x, y, w, h, simulation|
      if image.caption and simulation
        caption = NormalText.new(image.caption)
        caption.prop_set("size", @normal_font_size)
        set_font_family(caption)
        if image.horizontal_centering
          caption.do_horizontal_centering(canvas, x, y, w, h)
        end
        caption.compile(canvas, x, y, w, h)
        layout = caption.layout
        th = caption.height
      end
      if !simulation and layout
        canvas.draw_layout(layout, image.ox || x, y) # dirty!!!
      end
      [x, y + space + th, w, h - space - th]
    end
  end
end
