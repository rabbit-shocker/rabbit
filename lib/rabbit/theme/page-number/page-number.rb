proc_name = "page-number"

match(Page) do |pages|
  pages.delete_post_draw_proc_by_name(proc_name)

  pages.add_post_draw_proc(proc_name) do |page, canvas, x, y, w, h, simulation|
    unless simulation
      text = "#{canvas.current_index}/#{canvas.page_size - 1}"
      text = %Q[<span size="xx-large">#{text}</span>]
      layout, text_width, text_height = make_layout(canvas, text)
      layout.set_width(w * Pango::SCALE)
      layout.set_alignment(Pango::Layout::ALIGN_RIGHT)
      num_y = canvas.height - @bottom_margin - text_height
      draw_layout(canvas, layout, x, num_y)
    end
    [x, y, w, h]
  end
end

