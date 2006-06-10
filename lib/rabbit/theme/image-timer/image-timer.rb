theme_exit unless display?

add_image_path("rabbit-images")

proc_name = "image-timer"
init_proc_name_prefix = "image-timer-init"

@image_timer_limit ||= canvas.title_slide.allotted_time
if @image_timer_limit.nil?
  theme_exit("must specify @image_timer_limit!! (sec)")
end

if @image_timer_auto_update.nil?
  @image_timer_auto_update = true
end

if @image_timer_auto_scroll.nil?
  @image_timer_auto_scroll = false
end

@image_timer_auto_scroll_direction ||= :left

@image_timer_image ||= "kame.png"
@image_timer_interval ||= 5
@image_timer_space_ratio ||= 1.0 / 12.0

@image_timer_limit_time = nil

@image_time_auto_updating = false

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  stop_auto_reload_timer

  break if @image_timer_uninstall

  init_proc_name = "#{init_proc_name_prefix}.#{canvas.__id__}"
  slides.add_pre_draw_proc(init_proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      @image_timer_limit_time ||= Time.now + @image_timer_limit
      if @image_timer_auto_update && !@image_timer_auto_updating
        @image_timer_auto_updating = true
        start_auto_reload_timer(@image_timer_interval)
      end
      slide.delete_post_draw_proc_by_name(init_proc_name)
    end
    [x, y, w, h]
  end

  loader = ImageLoader.new(find_file(@image_timer_image))

  max_width = nil
  base_y = nil

  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    if simulation
      image_height = canvas.height * @image_timer_space_ratio
      loader.resize(nil, image_height)
      max_width = canvas.width - slide.margin_left
      max_width = max_width - slide.margin_right - loader.width
      base_y = canvas.height - slide.margin_bottom - loader.height
    else
      rest_time = @image_timer_limit_time - Time.now
      ratio = 1 - (rest_time.to_i / @image_timer_limit.to_f)
      base_x = slide.margin_left + max_width * ratio

      loader.draw(canvas, base_x, base_y)

      if @image_timer_auto_scroll
        if canvas.slide_size < 3
          slide_ratio = 1
        else
          slide_ratio = (canvas.current_index - 1.0) / (canvas.slide_size - 2.0)
          next_slide_ratio = (canvas.current_index) / (canvas.slide_size - 2.0)
        end

        if ratio > slide_ratio and !canvas.last_slide? and
            (canvas.current_index > 1 or
               (canvas.current_index == 1 and ratio > next_slide_ratio))
          auto_scroll_ratio = next_slide_ratio - slide_ratio
          auto_scroll_ratio *= @image_timer_interval
          case @image_timer_auto_scroll_direction
          when :top
            canvas.adjustment_y += auto_scroll_ratio
            canvas.activate("NextSlide") if canvas.adjustment_y > 1
          when :bottom
            canvas.adjustment_y -= auto_scroll_ratio
            canvas.activate("NextSlide") if canvas.adjustment_y < -1
          when :right
            canvas.adjustment_x -= auto_scroll_ratio
            canvas.activate("NextSlide") if canvas.adjustment_x < -1
          else
            canvas.adjustment_x += auto_scroll_ratio
            canvas.activate("NextSlide") if canvas.adjustment_x > 1
          end
        end
      end
    end
    [x, y, w, h]
  end
end
