theme_exit if print?

proc_name = "timer"
init_proc_name_prefix = "timer_init"

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

@timer_over_color ||= "red"
@timer_interval ||= 1

@timer_limit_time = nil
@@timer_auto_update_thread = nil

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)

  stop_auto_reload_thread

  break if @timer_uninstall
  
  if @timer_auto_update
    start_auto_reload_thread(@timer_interval)
  end
  
  init_proc_name = "#{init_proc_name_prefix}.#{canvas.__id__}"
  slides.add_pre_draw_proc(init_proc_name) do |slide, canvas, x, y, w, h, simulation|
    if @timer_limit_time.nil?
      @timer_limit_time = Time.now + @timer_limit
    end
    slide.delete_pre_draw_proc_by_name(init_proc_name)
    [x, y, w, h]
  end

  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      rest_time = @timer_limit_time - Time.now
      text = "%s%02d:%02d" % split_to_minute_and_second(rest_time)
      props = @timer_props.dup
      props["color"] = @timer_over_color if rest_time < 0
      text = %Q[<span #{to_attrs(props)}>#{text}</span>]
      layout, text_width, text_height = canvas.make_layout(text)
      layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @margin_bottom - text_height
      canvas.draw_layout(layout, x, num_y)
    end
    [x, y, w, h]
  end
end

def split_to_minute_and_second(number)
  if number >= 0
    sign = " "
  else
    sign = "-"
  end
  [sign, *number.abs.divmod(60)]
end
