@foot_text_block_line_color ||= "#ff9933"
@foot_text_block_line_width ||= 1
@foot_text_block_line_length_ratio ||= 0.5

match("**", FootTextBlock) do
  name = "foot-text-block"
  space = @space / 2.0

  delete_pre_draw_proc_by_name(name)
  each do |block|
    unless block.elements.empty?
      block.margin_with(:top => space * 3)

      block.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
        unless simulation
          args = [
            x,
            y - space * 2,
            (x + w * @foot_text_block_line_length_ratio).ceil,
            y - space * 2,
            @foot_text_block_line_color,
            {:line_width => @foot_text_block_line_width},
          ]
          canvas.draw_line(*args)
        end
        [x, y, w, h]
      end
    end
  end
end

match("**", FootText) do
  each do |text|
    if text["order_added"]
      order_text = text.elements.first
    else
      order_text = Text.new("(*#{text.order})")
      text.unshift(order_text)
      text["order_added"] = true
    end
    order_text.prop_set("foreground", "blue")
    order_text.prop_set("size", @script_font_size)
    order_text.prop_set("rise", (@script_font_size / 2.0).ceil)
  end
end

match("**", Footnote) do
  prop_set("foreground", "blue")
  each do |note|
    note.text = "(*#{note.order})"
  end
end
