proc_name = "clock"
init_proc_name = "clock_init"

if @clock_auto_update.nil?
  @clock_auto_update = true
end

@clock_props ||= {
  "size" => @xx_small_font_size,
  "font_family" => @default_font_family,
}

match(Slide) do |slides|

  slides.add_pre_draw_proc(init_proc_name) do |slide, canvas, x, y, w, h, simulation|
    if @clock_auto_update and
        slide.user_property["clock.thread"].nil?
      slide.user_property["clock.thread"] = Thread.new do
        loop do
          sleep(1)
          # break if slide.post_draw_procs(proc_name).nil?
          canvas.redraw
        end
      end
    end
    slide.delete_pre_draw_proc_by_name(init_proc_name)
    [x, y, w, h]
  end

  slides.delete_post_draw_proc_by_name(proc_name)

  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      text = Time.now.strftime('%H:%M:%S')
      text = %Q[<span #{to_attrs(@clock_props)}>#{text}</span>]
      layout, text_width, text_height = canvas.make_layout(text)
      layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @bottom_margin - text_height
      canvas.draw_layout(layout, x, num_y)
    end
    [x, y, w, h]
  end
end
