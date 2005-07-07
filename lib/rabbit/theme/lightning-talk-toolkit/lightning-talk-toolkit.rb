@very_huge_font_size ||= screen_size(15 * Pango::SCALE)
@lightning_talk_background_color ||= "white"
@lightning_talk_contact_information ||= nil
@lightning_talk_font_size ||= @x_small_font_size
@lightning_talk_as_large_as_possible ||= false
@lightning_talk_wrap_mode ||= Pango::Layout::WRAP_WORD
@lightning_talk_uninstall ||= false

def lightning_talk_slide(slides, proc_name)
  slides = to_element_container(slides)
  
  slides.left_margin = @left_margin
  slides.right_margin = @right_margin
  slides.top_margin = @top_margin
  slides.bottom_margin = @bottom_margin

  if @lightning_talk_uninstall
    slides.delete_post_draw_proc_by_name(proc_name)
    return
  end

  slides.clear_pre_draw_procs
  slides.clear_post_draw_procs
  
  slides.vertical_centering = true
  slides.horizontal_centering = true

  slides.add_pre_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      args = [
        true,
        x - @left_margin,
        y - @top_margin,
        w + @left_margin + @right_margin,
        h + @top_margin + @bottom_margin,
        @lightning_talk_background_color,
      ]
      canvas.draw_rectangle(*args)
    end
    [x, y, w, h]
  end
  
  if @lightning_talk_contact_information
    slides.delete_post_draw_proc_by_name(proc_name)

    slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
      unless simulation
        text = @lightning_talk_contact_information
        text = %Q[<span size="#{@lightning_talk_font_size}">#{text}</span>]
        layout, text_width, text_height = canvas.make_layout(text)
        layout.set_width(w * Pango::SCALE)
        layout.set_alignment(Pango::Layout::ALIGN_RIGHT)
        num_y = canvas.height - @bottom_margin - text_height
        canvas.draw_layout(layout, x, num_y)
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
  if @lightning_talk_as_large_as_possible
    max = (canvas.height - @top_margin - @bottom_margin) * Pango::SCALE
    width = (canvas.width - @left_margin - @right_margin) * Pango::SCALE
    heads.each do |head|
      size = head.prop_get("size").value
      loop do
        new_size = (size * 1.05).ceil
        head.prop_set("size", new_size)
        layout, text_width, text_height = canvas.make_layout(head.markuped_text)
        layout.width = width
        layout.wrap = @lightning_talk_wrap_mode
        if layout.size[1] > max or layout.size[0] > width
          break
        end
        size = new_size
      end
      head.prop_set("size", size)
    end
  end
  
  orig_x = orig_y = orig_w = orig_h = nil
  heads.add_pre_draw_proc(proc_name) do |head, canvas, x, y, w, h, simulation|
    orig_x, orig_y, orig_w, orig_h = x, y, w, h
    [x, y, w, h]
  end
  heads.add_post_draw_proc(proc_name) do |head, canvas, x, y, w, h, simulation|
    if head.empty?
      [orig_x, orig_y, orig_w, orig_h]
    else
      [x, y, w, h]
    end
  end
end

def lightning_talk_slide?(slide)
  slide.body.empty? or
    (slide.headline.empty? and
       slide.body.elements.all? {|elem| elem.is_a?(Image)})
end
