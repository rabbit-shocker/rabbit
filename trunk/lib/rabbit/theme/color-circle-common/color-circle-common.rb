def color_circle_slide(slides, name="color-circle-slide")
  slides.delete_pre_draw_proc_by_name(name)

  slides.add_pre_draw_proc(name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      cx = w * 0.22
      cy = h * 0.72
      r = w * 0.35
      canvas.draw_circle_by_radius(true, cx, cy, r, @color_circle_light_color)
      r = w * 0.34
      canvas.draw_circle_by_radius(true, cx, cy, r, @color_circle_bright_color)
      r = w * 0.28
      canvas.draw_circle_by_radius(true, cx, cy, r, @color_circle_light_color)
    end
    [x, y, w, h]
  end
end

def color_circle_title(titles, name="color-circle-title")
  titles.delete_pre_draw_proc_by_name(name)
  titles.delete_post_draw_proc_by_name(name)

  margin = canvas.width * 0.2
  line_width = {:line_width => screen_size(0.1)}
  titles.margin_top = screen_y(1)
  titles.margin_left = margin
  titles.margin_bottom = canvas.height * 0.1
  titles.add_pre_draw_proc(name) do |title, canvas, x, y, w, h, simulation|
    unless simulation
      ly = y + title.first_line_height
      canvas.draw_line(x, ly, x + w, ly, "black", line_width)

      base_h = title.first_line_height * 0.8
      cx = margin * 0.25
      cy = y + base_h * 0.8
      r = base_h * 0.3
      rest = margin - cx
      ratio = rest / (r * 2.5)
      canvas.draw_circle_by_radius(false, cx, cy, r, @color_circle_color,
                                   line_width)
      cx += r * ratio
      canvas.draw_circle_by_radius(true, cx, cy, r, @color_circle_color)
      cx += r * ratio
      canvas.draw_circle_by_radius(true, cx, cy, r, @color_circle_color)
    end
    [x, y, w, h]
  end
end
