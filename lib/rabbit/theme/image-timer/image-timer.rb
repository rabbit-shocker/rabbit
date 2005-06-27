theme_exit if print?

proc_name = "image-timer"
init_proc_name = "image-timer-init"
thread_property_name = "image-timer.thread"
thread_stop_property_name = "stop"

if @image_timer_limit.nil?
  raise "must specify @timer_limit!! (sec)"
end

if @image_timer_auto_update.nil?
  @image_timer_auto_update = true
end

@image_timer_image ||= "turtle.png"

match(Slide) do |slides|

  @image_timer_limit_time = nil

  slides.delete_post_draw_proc_by_name(init_proc_name)
  slides.delete_post_draw_proc_by_name(proc_name)
  slides.each do |slide|
    thread = slide.user_property[thread_property_name]
    if thread
      thread[thread_stop_property_name] = true
      slide.user_property[thread_property_name] = nil
    end
  end

  break if @image_timer_uninstall
  
  slides.add_pre_draw_proc(init_proc_name) do |slide, canvas, x, y, w, h, simulation|
    if @image_timer_limit_time.nil?
      @image_timer_limit_time = Time.now + @image_timer_limit
      if @image_timer_auto_update and
          slide.user_property[thread_property_name].nil?
        @image_timer_stop = false
        thread = Thread.new do
          loop do
            sleep(1)
            break if thread[thread_stop_property_name]
            canvas.redraw
          end
        end
        slide.user_property[thread_property_name] = thread
      end
    end
    slide.delete_post_draw_proc_by_name(init_proc_name)
    [x, y, w, h]
  end

  loader = ImageLoader.new(search_file(@image_timer_image))

  initialized = false
  max_width = nil
  start_base_x = nil
  end_base_x = nil
  base_y = nil

  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      unless initialized
        image_height = slide.height / 8
        loader.resize(nil, image_height)
        max_width = canvas.width - @left_margin - @right_margin
        base_y = canvas.height - @bottom_margin - loader.height
        initialized = true
      end
      rest_time = @image_timer_limit_time - Time.now
      ratio = 1 - (rest_time.to_i / @image_timer_limit.to_f)
      base_x = @left_margin + max_width * ratio
      canvas.draw_pixbuf(loader.pixbuf, base_x, base_y)
    end
    [x, y, w, h]
  end
end
