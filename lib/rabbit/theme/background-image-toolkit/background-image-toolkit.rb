def apply_background_image_property(element, options={})
  proc_name = options[:proc_name] || "background-image"
  compute_initial_geometry = options[:compute_initial_geometry]

  element.delete_pre_draw_proc_by_name(proc_name)

  background_image = element["background-image"]
  return if background_image.nil?

  properties = {}
  element.user_property.each do |name, value|
    if /\Abackground-image-/ =~ name
      properties[$POSTMATCH.gsub(/-/, '_')] = value
    end
  end
  image = Image.new(canvas.full_path(background_image), properties)
  right = properties["align"] == "right"
  element_margin_right = 0
  unless right
    image.horizontal_centering = true
  end
  image.vertical_centering = true
  element.add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
    if simulation
      if compute_initial_geometry
        _x, _y, _w, _h = compute_initial_geometry.call(canvas, x, y, w, h)
      else
        _x, _y, _w, _h = x, y, w, h
      end
      old_geometry = [_x, _y, _w, _h]
      image.compile(canvas, _x, _y, _w, _h)
      if image.do_vertical_centering?
        adjust_height = ((_h - image.height - image.padding_bottom) / 2.0).ceil
        if _y + adjust_height > 0
          _y += adjust_height
          _h -= adjust_height
        end
      end
      if right
        element_margin_right = image.width
        _x = _w - element_margin_right - image.margin_right
      end

      if old_geometry != [_x, _y, _w, _h]
        image.compile(canvas, _x, _y, _w, _h)
      end

      if image.do_horizontal_centering?
        image.do_horizontal_centering(canvas, _x, _y, _w, _h)
      end
    end
    image.draw(simulation)
    [x, y, w - element_margin_right, h]
  end

  element.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
    [x, y, w + element_margin_right, h]
  end
end
