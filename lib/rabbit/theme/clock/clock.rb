proc_name = "clock"

@clock_font_size ||= @xx_small_font_size

match(Page) do |pages|
  pages.delete_post_draw_proc_by_name(proc_name)

  pages.add_post_draw_proc(proc_name) do |page, canvas, x, y, w, h, simulation|
    unless simulation
      text = Time.now.strftime('%l:%m:%S')
      text = %Q[<span size="#{@page_number_font_size}">#{text}</span>]
      layout, text_width, text_height = make_layout(canvas, text)
      layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @bottom_margin - text_height
      draw_layout(canvas, layout, x, num_y)
    end
    [x, y, w, h]
  end
end

