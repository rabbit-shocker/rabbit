theme_exit unless display?

proc_name = "clock"

if @clock_auto_update.nil?
  @clock_auto_update = true
end

@@clock_auto_update_thread = nil

@clock_props ||= {
  "size" => @xx_small_font_size,
  "font_family" => @font_family,
}

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  stop_auto_reload_thread

  break if @clock_uninstall

  if @clock_auto_update
    start_auto_reload_thread(1)
  end
  
  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      text = Time.now.strftime('%H:%M:%S')
      text = %Q[<span #{to_attrs(@clock_props)}>#{text}</span>]
      layout, text_width, text_height = canvas.make_layout(text)
      layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @margin_bottom - text_height
      canvas.draw_layout(layout, x, num_y)
    end
    [x, y, w, h]
  end
end
