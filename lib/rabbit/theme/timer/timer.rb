proc_name = "timer"
init_proc_name = "timer_init"

if @timer_limit.nil?
  raise "must specify @timer_limit!! (sec)"
end

@timer_font_size ||= @xx_small_font_size

@@timer_threads ||= []

match(Page) do |pages|

  @timer_limit_time = nil

  @@timer_threads.each do |thread|
    thread.exit
  end
  @@timer_threads.clear
  
  pages.add_pre_draw_proc(init_proc_name) do |page, canvas, x, y, w, h, simulation|
    if @timer_limit_time.nil?
      @timer_limit_time = Time.now + @timer_limit
      @@timer_threads << Thread.new do
        loop do
          sleep(1)
          canvas.redraw
        end
      end
      pages.delete_post_draw_proc_by_name(init_proc_name)
    end
    [x, y, w, h]
  end

  pages.delete_post_draw_proc_by_name(proc_name)

  pages.add_post_draw_proc(proc_name) do |page, canvas, x, y, w, h, simulation|
    unless simulation
      text = "%d:%d" % split_to_minute_and_second(@timer_limit_time - Time.now)
      text = %Q[<span size="#{@page_number_font_size}">#{text}</span>]
      layout, text_width, text_height = make_layout(canvas, text)
      layout.set_width(w * Pango::SCALE)
      num_y = canvas.height - @bottom_margin - text_height
      draw_layout(canvas, layout, x, num_y)
    end
    [x, y, w, h]
  end
end

def split_to_minute_and_second(number)
  number.divmod(60)
end
