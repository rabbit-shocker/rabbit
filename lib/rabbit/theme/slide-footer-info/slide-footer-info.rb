name = "slide-footer-info"

@slide_footer_info_line_color ||= "#666"
@slide_footer_info_line_width ||= screen_y(0.1)
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
      bottom = canvas.height - @margin_bottom
      line_width = {:line_width => @slide_footer_info_line_width}
      canvas.draw_line(0, bottom, canvas.width, bottom,
                       @slide_footer_info_line_color,
                       @slide_footer_info_line_params.merge(line_width))

      props = {
        "font_family" => @font_family,
        "size" => @slide_footer_info_text_size,
        "color" => @slide_footer_info_text_color,
      }
      left_layout = right_layout = nil
      if @slide_footer_info_left_text
        left_layout = make_layout(span(props, @slide_footer_info_left_text))
      end
      if @slide_footer_info_right_text
        right_layout = make_layout(span(props, @slide_footer_info_right_text))
      end

      layouts = [left_layout, right_layout].compact
      unless layouts.empty?
        bottom_space = @margin_bottom - @slide_footer_info_line_width
        max_height = layouts.collect {|layout| layout.pixel_size[1]}.max
        bottom_space -= (bottom_space - max_height) / 2
        bottom = canvas.height - bottom_space

        if left_layout
          canvas.draw_layout(left_layout, @slide_footer_info_x_margin, bottom)
        end

        if right_layout
          text_width, text_height = right_layout.pixel_size
          right_text_x = canvas.width - text_width - @slide_footer_info_x_margin
          canvas.draw_layout(right_layout, right_text_x, bottom)
        end
      end
    end
    [x, y, w, h]
  end
end
