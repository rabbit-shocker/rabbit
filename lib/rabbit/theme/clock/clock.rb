proc_name = "clock"
init_proc_name = "clock_init"

if @clock_auto_update.nil?
  @clock_auto_update = true
end

@clock_font_size ||= @xx_small_font_size

match(Page) do |pages|

  pages.add_pre_draw_proc(init_proc_name) do |page, canvas, x, y, w, h, simulation|
    if @clock_auto_update and
        page.user_property["clock.thread"].nil?
      page.user_property["clock.thread"] = Thread.new do
        loop do
          sleep(1)
          # break if page.post_draw_procs(proc_name).nil?
          canvas.redraw
        end
      end
    end
    page.delete_post_draw_proc_by_name(init_proc_name)
    [x, y, w, h]
  end

  pages.delete_post_draw_proc_by_name(proc_name)

  pages.add_post_draw_proc(proc_name) do |page, canvas, x, y, w, h, simulation|
    unless simulation
      text = Time.now.strftime('%H:%M:%S')
      text = %Q[<span size="#{@page_number_font_size}">#{text}</span>]
      layout, text_width, text_height = make_layout(canvas, text)
      layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @bottom_margin - text_height
      draw_layout(canvas, layout, x, num_y)
    end
    [x, y, w, h]
  end
end
