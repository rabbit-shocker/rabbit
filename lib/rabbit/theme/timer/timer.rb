theme_exit unless display?

proc_name = "timer"
init_proc_name_prefix = "timer_init"

@timer_limit ||= canvas.allotted_time
if @timer_limit.nil?
  theme_exit("must specify @timer_limit!! (sec)")
end

if @timer_auto_update.nil?
  @timer_auto_update = true
end

@timer_props ||= {
  "size" => @xx_small_font_size,
  "font_family" => @font_family,
}
@timer_props.delete("font_family") unless @timer_props["font_family"]

@timer_color ||= "#0006"
@timer_over_color ||= "#f006"
@timer_interval ||= 1

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)

  stop_auto_redraw_timer

  break if @timer_uninstall
  
  if @timer_auto_update
    start_auto_redraw_timer(@timer_interval)
  end
  
  init_proc_name = "#{init_proc_name_prefix}.#{canvas.__id__}"
  slides.add_pre_draw_proc(init_proc_name) do |slide, canvas, x, y, w, h, simulation|
    canvas.start_timer(@timer_limit) if canvas.rest_time
    slide.delete_pre_draw_proc_by_name(init_proc_name)
    [x, y, w, h]
  end

  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      rest_time = canvas.rest_time
      text = "%s%02d:%02d" % Utils.split_number_to_minute_and_second(rest_time)
      text = Text.new(text)
      text.font @timer_props
      set_font_family(text)
      text.compile(canvas, x, y, w, h)
      text.layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @margin_bottom - text.height
      args = [text.layout, x, num_y]
      if rest_time < 0
        args << @timer_over_color
      else
        args << @timer_color
      end
      canvas.draw_layout(*args)
    end
    [x, y, w, h]
  end
end
