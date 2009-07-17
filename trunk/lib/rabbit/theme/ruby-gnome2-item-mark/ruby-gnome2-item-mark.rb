include_theme("default-item-mark")

def setup_ruby_gnome2_item_paragraph(paragraphs, name, color, line_color,
                                     space_ratio)
  paragraphs.delete_pre_draw_proc_by_name(name)
  paragraphs.delete_post_draw_proc_by_name(name)

  paragraphs.prop_set("foreground", color)
  space = @space * space_ratio

  post_draw_proc = Proc.new do |paragraph, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + w, y + space, line_color)
    end
    [x, y, w, h]
  end
  paragraphs.add_post_draw_proc(name, &post_draw_proc)
end


slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  space = @space * (5 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 1) + [Paragraph])) do |paragraphs|
  setup_ruby_gnome2_item_paragraph(paragraphs, "item1-paragraph",
                                   @ruby_gnome2_color,
                                   @ruby_gnome2_line_color,
                                   (3 / 8.0))
end

match(*(slide_body + (item_list_item * 2))) do |items|
  compute_mark_space = Proc.new do |mark_width|
    mark_width * 0.5
  end
  setup_default_item_mark(items, "item2", 2, 2, (3 / 4.0),
                          @ruby_gnome2_item_mark_color,
                          "mark_space" => compute_mark_space,
                          "type" => "circle")
end

match(*(slide_body + (item_list_item * 3))) do |items|
  setup_default_item_mark(items, "item3", 1, 1, (1 / 4.0),
                          @ruby_gnome2_item_mark_color,
                          "type" => "rectangle")
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1) + [Paragraph])) do |paragraphs|
  setup_ruby_gnome2_item_paragraph(paragraphs, "enum1-paragraph",
                                   @ruby_gnome2_color,
                                   @ruby_gnome2_line_color,
                                   (3 / 8.0))
end
