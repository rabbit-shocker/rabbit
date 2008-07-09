@description_term_line_color ||= "#ff9900"

match("**", DescriptionTerm) do
  name = "description-term-line"

  space = @space / 2.0

  margin_with(:bottom => space * 3)
  delete_post_draw_proc_by_name(name)
  add_post_draw_proc(name) do |term, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + term.width, y + space,
                       @description_term_line_color)
    end
    [x, y, w, h]
  end
end


slide_body = [Slide, Body]

match(*(slide_body + [DescriptionList])) do
  margin_with(:top => @space)
end

desc_list_item = [DescriptionList, DescriptionListItem]

match(*(slide_body + desc_list_item)) do
  space = @space * (3 / 4.0)
  margin_with(:bottom => space)
end

match(*(slide_body + (desc_list_item * 2))) do
  space = @space * (2 / 4.0)
  margin_with(:bottom => space)
end

match(*(slide_body + (desc_list_item * 3))) do
  space = @space * (1 / 4.0)
  margin_with(:bottom => space)
end

desc_list_content = desc_list_item + [DescriptionContent]

match(*(slide_body + desc_list_content)) do
  name = "description-content-indent1"

  space = @normal_font_size / Pango::SCALE
  indent(space, name)
end

match(*(slide_body + desc_list_content * 2)) do
  name = "description-content-indent2"

  space = @small_font_size / Pango::SCALE
  indent(space, name)
end

match(*(slide_body + desc_list_content * 3)) do
  name = "description-content-indent3"

  space = @x_small_font_size / Pango::SCALE
  indent(space, name)
end
