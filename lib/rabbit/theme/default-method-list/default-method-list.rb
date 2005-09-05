match("**", MethodListItem, Paragraph) do |texts|
  name = "method-list-item-paragraph"

  texts.delete_pre_draw_proc_by_name(name)
  texts.delete_post_draw_proc_by_name(name)
  
  space = @normal_font_size / Pango::SCALE
  indent(texts, space, name)
end
