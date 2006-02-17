include_theme("blue-circle-config")

@foot_text_block_line_color = @blue_circle_blue
@foot_text_block_line_length_ratio = 0.8

match("**", FootTextBlock) do
  name = "foot-text-block-indent"
  indent = canvas.width * 0.1

  delete_pre_draw_proc_by_name(name)
  add_pre_draw_proc(name) do |block, canvas, x, y, w, h, simulation|
    [x + indent, y, w - indent, h]
  end
end

include_theme("default-foot-text")
