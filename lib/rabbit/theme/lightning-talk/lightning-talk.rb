proc_name = "lightning-talk"

@very_huge_font_size ||= screen_size(15 * Pango::SCALE)
@lightning_talk_contact_information ||= nil
@lightning_talk_font_size ||= @x_small_font_size
@lightning_talk_as_large_as_possible ||= false

match(TitleSlide) do |slides|
  slides.horizontal_centering = true
  slides.vertical_centering = true

  slides.left_margin = @left_margin
  slides.right_margin = @right_margin
  slides.top_margin = @top_margin
  slides.bottom_margin = @bottom_margin
end

match(TitleSlide, "*") do |elems|
  elems.prop_set("size", @large_font_size)
end

match(TitleSlide, Title) do |titles|
  titles.prop_set("size", @huge_font_size)
  titles.prop_set("weight", "heavy")

  space = screen_size(5)
  titles.add_post_draw_proc do |title, canvas, x, y, w, h, simulation|
    if title.next_element.is_a?(Subtitle)
      [x, y, w, h]
    else
      [x, y + space, w, h - space]
    end
  end
end

match(TitleSlide, Subtitle) do |titles|
  titles.prop_set("size", @normal_font_size)

  space = screen_size(5)
  titles.add_post_draw_proc do |title, canvas, x, y, w, h, simulation|
    if title.next_element.is_a?(Subtitle)
      [x, y, w, h]
    else
      [x, y + space, w, h - space]
    end
  end
end

match(TitleSlide, ContentSource) do |titles|
  titles.prop_set("size", @small_font_size)
  titles.prop_set("style", "italic")
end

match(TitleSlide, Institution) do |titles|
  titles.prop_set("size", @normal_font_size)
  titles.prop_set("style", "italic")
end


match(Slide) do |slides|
  slides.left_margin = @left_margin
  slides.right_margin = @right_margin
  slides.top_margin = @top_margin
  slides.bottom_margin = @bottom_margin

  slides.vertical_centering = true
  slides.horizontal_centering = true

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

match(Slide, HeadLine) do |heads|
  heads.prop_set("size", @very_huge_font_size)
  if @lightning_talk_as_large_as_possible
    heads.each do |head|
      size = head.prop_get("size").value
      loop do
        new_size = size + screen_size(0.1 * Pango::SCALE)
        text = %Q[<span size="#{new_size}">#{head.text}</span>]
        layout, text_width, text_height = canvas.make_layout(text)
        layout.set_width(canvas.width * Pango::SCALE)
        break if layout.size[1] / Pango::SCALE > canvas.height * 0.7
        size = new_size
      end
      head.prop_set("size", size)
    end
  end
end

match("**", Emphasis) do |texts|
  texts.prop_set("foreground", "red")
  texts.prop_set("weight", "heavy")
end
