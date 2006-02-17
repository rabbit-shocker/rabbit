match("**", MethodListItem, Paragraph) do
  name = "method-list-item-paragraph"

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)
  
  space = @normal_font_size / Pango::SCALE
  indent(space, name)
end
