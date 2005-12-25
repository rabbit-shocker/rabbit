proc_name = "slide-number"

@slide_number_props ||= {
  "size" => @xx_small_font_size,
  "font_family" => @font_family,
}

@slide_number_color || nil

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)

  break if @slide_number_uninstall
  
  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      text = Text.new("#{canvas.current_index}/#{canvas.slide_size - 1}")
      text.font @slide_number_props
      text.align = Pango::Layout::ALIGN_RIGHT
      text.compile(canvas, x, y, w, h)
      layout = text.layout
      layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @margin_bottom - text.height
      canvas.draw_layout(text.layout, x, num_y, @slide_number_color)
    end
    [x, y, w, h]
  end
end

