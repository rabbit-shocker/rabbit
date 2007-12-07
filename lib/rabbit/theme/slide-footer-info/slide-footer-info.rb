name = "slide-footer-info"

@slide_footer_info_line_color ||= "#666"
@slide_footer_info_line_params ||= {
  :pattern => {
    :base => [0, 0, canvas.width, 0],
    :type => :linear,
    :color_stops => [
                     [0.0, 1, 1, 1],
                     [0.3, 0, 0, 0],
                     [0.7, 0, 0, 0],
                     [1.0, 1, 1, 1],
                    ],
  }
}
@slide_footer_info_text_size ||= screen_size(1.5 * Pango::SCALE)
@slide_footer_info_x_margin ||= screen_x(1)
@slide_footer_info_text_color ||= "#666"

match(SlideElement) do
  delete_pre_draw_proc_by_name(name)

  break if @slide_footer_info_uninstall

  add_pre_draw_proc(name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      bottom = canvas.height * 0.95
      canvas.draw_line(0, bottom, canvas.width, bottom,
                       @slide_footer_info_line_color,
                       @slide_footer_info_line_params)

      bottom = canvas.height * 0.96
      props = {
        "font_family" => @font_family,
        "size" => @slide_footer_info_text_size,
        "color" => @slide_footer_info_text_color,
      }

      if @slide_footer_info_left_text
        layout = make_layout(span(props, @slide_footer_info_left_text))
        canvas.draw_layout(layout, @slide_footer_info_x_margin, bottom)
      end

      if @slide_footer_info_right_text
        layout = make_layout(span(props, @slide_footer_info_right_text))
        text_width, text_height = layout.pixel_size
        right_text_x = canvas.width - text_width - @slide_footer_info_x_margin
        canvas.draw_layout(layout, right_text_x, bottom)
      end
    end
    [x, y, w, h]
  end
end
