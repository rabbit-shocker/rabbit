match(TitlePage, Title) do |titles|
  @title_shadow_color ||= "#6f6f6f"

  shadow_layout = nil
  move_x = nil
  move_y = nil

  titles.add_pre_draw_proc do |title, canvas, x, y, w, h, simulation|
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
        shadow_title.prop_set("foreground", @title_shadow_color)
        shadow_layout, _, _ = make_layout(canvas, shadow_title.markuped_text)
        shadow_layout.set_width(w * Pango::SCALE)
        if title.do_holizontal_centering?
          shadow_layout.set_alignment(Pango::Layout::ALIGN_CENTER)
        end
      end
      draw_layout(canvas, shadow_layout, x + move_x, y + move_y)
    end
    [x, y, w, h]
  end
end
