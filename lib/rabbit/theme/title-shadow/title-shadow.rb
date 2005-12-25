proc_name = "title-shadow"

match(TitleSlide, Title) do |titles|
  @title_shadow_color ||= "#6f6f6fcc"

  shadow_layout = nil
  move_x = nil
  move_y = nil

  titles.delete_pre_draw_proc_by_name(proc_name)

  titles.add_pre_draw_proc(proc_name) do |title, canvas, x, y, w, h, simulation|
    unless simulation
      if shadow_layout.nil?
        font_size = title.prop_get("size")
        if font_size
          font_size = font_size.value.to_f / Pango::SCALE
        else
          font_size = title.original_height
        end
        move_x = screen_x(font_size.to_f / screen_size(10))
        move_y = screen_y(font_size.to_f / screen_size(20))

        shadow_title = title.clone
        shadow_title.font :color => nil
        shadow_layout = canvas.make_layout(shadow_title.markuped_text)
        shadow_layout.set_width(w * Pango::SCALE)
        if title.do_horizontal_centering? or
            title.parent.do_horizontal_centering?
          shadow_layout.set_alignment(Pango::Layout::ALIGN_CENTER)
        end
      end
      args = [shadow_layout, x + move_x, y + move_y, @title_shadow_color]
      canvas.draw_layout(*args)
    end
    [x, y, w, h]
  end
end
