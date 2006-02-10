match("**", FoottextBlock) do
  name = "foottext-block"
  space = @space / 2.0
  color = "#33ff33"

  delete_pre_draw_proc_by_name(name)
  each do |block|
    unless block.elements.empty?
      block.margin_set(:top => space * 3)

      block.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
        unless simulation
          args = [
            x, y - space * 2, (x + w / 2.0).ceil, y - space * 2,
            color
          ]
          canvas.draw_line(*args)
        end
        [x, y, w, h]
      end
    end
  end
end

match("**", Foottext) do
  each do |text|
    if text.user_property["order_added"]
      order_text = text.elements.first
    else
      order_text = Text.new("(*#{text.order})")
      text.unshift(order_text)
      text.user_property["order_added"] = true
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
