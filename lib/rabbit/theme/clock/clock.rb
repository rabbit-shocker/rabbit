theme_exit if print?

proc_name = "clock"
init_proc_name = "clock_init"

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

  break if @clock_uninstall
  
  slides.add_pre_draw_proc(init_proc_name) do |slide, canvas, x, y, w, h, simulation|
    if @clock_auto_update and
        @@clock_auto_update_thread.nil?
      thread = Thread.new do
        loop do
          sleep(1)
          break if @@clock_auto_update_thread != thread
          canvas.redraw
        end
      end
      @@clock_auto_update_thread = thread
    end
    slide.delete_pre_draw_proc_by_name(init_proc_name)
    [x, y, w, h]
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
