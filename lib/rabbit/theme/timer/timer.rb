proc_name = "timer"
init_proc_name = "timer_init"

if @timer_limit.nil?
  raise "must specify @timer_limit!! (sec)"
end

if @timer_auto_update.nil?
  @timer_auto_update = true
end

@timer_font_size ||= @xx_small_font_size

match(Page) do |pages|

  @timer_limit_time = nil

  pages.add_pre_draw_proc(init_proc_name) do |page, canvas, x, y, w, h, simulation|
    if @timer_limit_time.nil?
      @timer_limit_time = Time.now + @timer_limit
      if @timer_auto_update and
          page.user_property["timer.thread"].nil?
        page.user_property["timer.thread"] = Thread.new do
          loop do
            sleep(1)
            # break if page.post_draw_proc(proc_name).nil?
            canvas.redraw
          end
        end
      end
    end
    page.delete_post_draw_proc_by_name(init_proc_name)
    [x, y, w, h]
  end

  pages.delete_post_draw_proc_by_name(proc_name)

  pages.add_post_draw_proc(proc_name) do |page, canvas, x, y, w, h, simulation|
    unless simulation
      rest_time = @timer_limit_time - Time.now
      text = "%s%02d:%02d" % split_to_minute_and_second(rest_time)
      text = %Q[<span size="#{@page_number_font_size}">#{text}</span>]
      layout, text_width, text_height = canvas.make_layout(text)
      layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @bottom_margin - text_height
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
