proc_name = "lightning-talk"

@very_huge_font_size ||= screen_size(15 * Pango::SCALE)
@lightning_talk_contact_information ||= nil
@lightning_talk_font_size ||= @x_small_font_size


match(TitlePage) do |pages|
  pages.horizontal_centering = true
  pages.vertical_centering = true

  pages.left_margin = @left_margin
  pages.right_margin = @right_margin
  pages.top_margin = @top_margin
  pages.bottom_margin = @bottom_margin
end

match(TitlePage, "*") do |elems|
  elems.prop_set("size", @large_font_size)
end

match(TitlePage, Title) do |titles|
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

match(TitlePage, Subtitle) do |titles|
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

match(TitlePage, ContentSource) do |titles|
  titles.prop_set("size", @small_font_size)
  titles.prop_set("style", "italic")
end

match(TitlePage, Institution) do |titles|
  titles.prop_set("size", @normal_font_size)
  titles.prop_set("style", "italic")
end


match(Page) do |pages|
  pages.left_margin = @left_margin
  pages.right_margin = @right_margin
  pages.top_margin = @top_margin
  pages.bottom_margin = @bottom_margin

  pages.vertical_centering = true
  pages.horizontal_centering = true

  if @lightning_talk_contact_information
    pages.delete_post_draw_proc_by_name(proc_name)

    pages.add_post_draw_proc(proc_name) do |page, canvas, x, y, w, h, simulation|
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

match(Page, HeadLine) do |heads|
  heads.prop_set("size", @very_huge_font_size)
end

match("**", Emphasis) do |texts|
  texts.prop_set("foreground", "red")
  texts.prop_set("weight", "heavy")
  texts.prop_set("size", @very_huge_font_size)
end
