def apply_background_image_property(element, options={})
  proc_name = options[:proc_name] || "background-image"
  compute_initial_geometry = options[:compute_initial_geometry]

  element.delete_pre_draw_proc_by_name(proc_name)

  if options.has_key?(:file_name)
    background_image = options[:file_name]
  else
    background_image = element["background-image"]
  end
  return if background_image.nil?

  properties = options[:properties] || {}
  element.user_property.each do |name, value|
    if /\Abackground-image-/ =~ name
      properties[$POSTMATCH.gsub(/-/, '_')] = value
    end
  end

  image = image_element(background_image, properties)
  align = properties["align"] || "center"
  vertical_align = properties["vertical_align"] || "middle"
  assign_box = properties["assign_box"]
  element_margin_right = 0
  case align
  when "center"
    image.horizontal_centering = true
  end
  case vertical_align
  when "middle"
    image.vertical_centering = true
  end

  layout = nil
  caption_height = 0
  caption_text = image.caption
  caption_text = nil if caption_text and caption_text.empty?

  element.add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
    if simulation
      if compute_initial_geometry
        _x, _y, _w, _h = compute_initial_geometry.call(canvas, x, y, w, h)
      else
        _x, _y, _w, _h = x, y, w, h
      end

      old_geometry = [_x, _y, _w, _h]
      image.compile(canvas, _x, _y, _w, _h)

      case vertical_align
      when "middle"
        adjust_height = ((_h - image.height - image.padding_bottom) / 2.0).ceil
        if _y + adjust_height > 0
          _y += adjust_height
          _h -= adjust_height
        end
      when "bottom"
        adjust_height =
          image.height + image.padding_bottom + element.margin_bottom
        _y = _h - adjust_height
        _h -= adjust_height
      end

      case align
      when "right"
        if assign_box
          element_margin_right = image.width +
            image.margin_left + image.margin_right +
            element.margin_right + element.padding_right
          element_margin_right += element.parent.margin_right if element.parent
        end
        _x = _w - image.width - image.margin_right
      when "center"
      end

      if old_geometry != [_x, _y, _w, _h]
        image.compile(canvas, _x, _y, _w, _h)
      end

      if image.do_horizontal_centering?
        image.do_horizontal_centering(canvas, _x, _y, _w, _h)
      end
    end
    image.draw(simulation)

    if caption_text
      # TODO: Should we move this to Image#draw and unify this and
      # similar code in lib/rabbit/theme/image/image.rb?
      if simulation
        caption = Text.new(caption_text)
        caption_font_size = image.caption_font_size
        caption_font_size = font_size(caption_font_size) if caption_font_size
        caption.prop_set("size",
                         caption_font_size || @image_caption_font_size)
        set_font_family(caption)
        if image.horizontal_centering
          caption.do_horizontal_centering(canvas, x, y, w, h)
        end
        caption.compile(canvas,
                        (image.x || x) + image.margin_left,
                        y + image.height + image.margin_top,
                        image.ow || w,
                        h - image.height)
        layout = caption.layout
        caption_height = caption.height
      end
      if !simulation and layout
        base_x = (image.x || x) + image.margin_left
        base_y = image.height + y + image.margin_top
        caption_color = image["caption-color"] || @image_caption_color
        canvas.draw_layout(layout,
                           base_x,
                           base_y + image.margin_bottom,
                           caption_color)
      end
    end

    [x, y, w - element_margin_right, h]
  end

  element.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
    [x, y, w + element_margin_right, h]
  end
end
