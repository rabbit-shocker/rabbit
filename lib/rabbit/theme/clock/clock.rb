theme_exit unless display?

proc_name = "clock"

if @clock_auto_update.nil?
  @clock_auto_update = true
end

@clock_props ||= {
  "size" => @xx_small_font_size,
  "font_family" => @font_family,
}
@clock_color ||= "#0009"

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  stop_auto_reload_timer

  break if @clock_uninstall

  if @clock_auto_update
    start_auto_reload_timer(1)
  end
  
  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      text = Text.new(Time.now.strftime('%H:%M:%S'))
      text.font @clock_props
      set_font_family(text)
      text.compile(canvas, x, y, w, h)
      text.layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @margin_bottom - text.height
      canvas.draw_layout(text.layout, x, num_y, @clock_color)
    end
    [x, y, w, h]
  end
end
