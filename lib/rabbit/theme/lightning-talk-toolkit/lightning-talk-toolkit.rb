@very_huge_font_size ||= screen_size(15 * Pango::SCALE)
@lightning_talk_background_color ||= "white"
@lightning_talk_contact_information ||= nil
@lightning_talk_contact_information_font_size ||= @x_small_font_size
@lightning_talk_contact_information_color ||= nil
@lightning_talk_as_large_as_possible ||= false
@lightning_talk_wrap_mode ||= Pango::Layout::WRAP_WORD
@lightning_talk_uninstall ||= false

def lightning_talk_slide(slides, proc_name)
  slides = to_element_container(slides)
  
  slides.margin_left = @margin_left
  slides.margin_right = @margin_right
  slides.margin_top = @margin_top
  slides.margin_bottom = @margin_bottom

  slides.delete_post_draw_proc_by_name(proc_name)
  return if @lightning_talk_uninstall

  slides.clear_pre_draw_procs
  slides.clear_post_draw_procs
  
  slides.vertical_centering = true
  slides.horizontal_centering = true

  slides.add_pre_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      args = [
        true,
        x - @margin_left,
        y - @margin_top,
        w + @margin_left + @margin_right,
        h + @margin_top + @margin_bottom,
        @lightning_talk_background_color,
      ]
      canvas.draw_rectangle(*args)
    end
    [x, y, w, h]
  end
  
  if @lightning_talk_contact_information
    slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
      unless simulation
        text = Text.new(@lightning_talk_contact_information)
        text.font :size => @lightning_talk_contact_information_font_size
        set_font_family(text)
        text.align = Pango::Layout::ALIGN_RIGHT
        text.compile(canvas, x, y, w, h)
        text.layout.set_width(slide.width * Pango::SCALE)
        text_x = @margin_left
        text_y = canvas.height - @margin_bottom - text.height
        canvas.draw_layout(text.layout, text_x, text_y,
                           @lightning_talk_contact_information_color)
      end
      [x, y, w, h]
    end
  end
end

def lightning_talk_headline(heads, proc_name)
  heads = to_element_container(heads)
  
  heads.delete_pre_draw_proc(proc_name)
  heads.delete_post_draw_proc(proc_name)

  return if @lightning_talk_uninstall
  
  heads.clear_pre_draw_procs
  heads.clear_post_draw_procs
  
  set_font_family(heads)
  heads.wrap_mode = @lightning_talk_wrap_mode
  
  heads.prop_set("size", @very_huge_font_size)

  max_height = (canvas.height - @margin_top - @margin_bottom) * Pango::SCALE
  max_width = (canvas.width - @margin_left - @margin_right) * Pango::SCALE
  heads.each do |head|
    computed = false
    orig_x = orig_y = orig_w = orig_h = nil
    head.add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      if simulation and
          @lightning_talk_as_large_as_possible and
          !computed
        computed = true
        size = head.prop_get("size").value
        loop do
          new_size = (size * 1.05).ceil
          head.prop_set("size", new_size)
          text = head.markuped_text
          layout, text_width, text_height = canvas.make_layout(text)
          layout.width = max_width
          layout.wrap = @lightning_talk_wrap_mode
          if layout.size[1] > max_height or layout.size[0] > max_width
            break
          end
          size = new_size
        end
        head.prop_set("size", size)
      end
      orig_x, orig_y, orig_w, orig_h = x, y, w, h
      [x, y, w, h]
    end

    head.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      if head.empty?
        [orig_x, orig_y, orig_w, orig_h]
      else
        [x, y, w, h]
      end
    end
  end
end

def lightning_talk_slide?(slide)
  slide.body.empty? or
    (slide.headline.empty? and
       slide.body.elements.all? {|elem| elem.is_a?(Image)})
end
