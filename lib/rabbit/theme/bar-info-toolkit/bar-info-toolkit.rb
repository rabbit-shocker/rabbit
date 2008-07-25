match(SlideElement) do |slides|
  slides.each do |slide|
    singleton_class = class << slide; self; end
    make_formatted_layout = Proc.new do |props, text|
      make_layout(span(props, text))
    end
    singleton_class.send(:define_method, :draw_bar_info) do |options|
      name = options[:name] || "bar-info"
      add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
        unless simulation
          line_width = options[:bar_line_width] || screen_y(0.1)
          line_color = options[:line_color] || "#666"
          line_params = options[:line_params] || {}
          left_text = options[:left_text]
          right_text = options[:right_text]
          text_position = options[:text_position] || :lower
          text_over_line = options[:text_over_line]
          text_size = options[:text_size] || screen_size(1.5 * Pango::SCALE)
          text_color = options[:text_color] || "#666"
          x_margin = options[:x_margin] || screen_x(1)

          base_y = options[:y] || (canvas.height - @margin_bottom)
          if text_position == :lower
            bar_y = base_y + line_width / 2
          else
            bar_y = base_y - line_width / 2
          end

          line_width_params = {:line_width => line_width}
          canvas.draw_line(0, bar_y, canvas.width, bar_y,
                           line_color, line_params.merge(line_width_params))

          props = {
            "font_family" => @font_family,
            "size" => text_size,
            "color" => text_color,
          }
          left_layout = right_layout = nil
          if left_text
            left_layout = make_formatted_layout.call(props, left_text)
          end
          if right_text
            right_layout = make_formatted_layout.call(props, right_text)
          end

          layouts = [left_layout, right_layout].compact
          unless layouts.empty?
            text_space = base_y
            max_height = layouts.collect {|layout| layout.pixel_size[1]}.max
            if text_position == :lower
              text_space -= line_width unless text_over_line
              text_space -= (text_space - max_height) / 2
              text_base_y = canvas.height - text_space
            else
              text_space -= line_width unless text_over_line
              text_space -= (text_space - max_height) / 2
              text_base_y = base_y - text_space
            end

            if left_layout
              canvas.draw_layout(left_layout, x_margin, text_base_y)
            end

            if right_layout
              text_width, text_height = right_layout.pixel_size
              right_text_x = canvas.width - text_width - x_margin
              canvas.draw_layout(right_layout, right_text_x, text_base_y)
            end
          end
        end
        [x, y, w, h]
      end
    end
  end
end
