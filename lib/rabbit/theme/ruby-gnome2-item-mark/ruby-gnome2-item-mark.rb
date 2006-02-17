include_theme("ruby-gnome2-config")

include_theme("default-item-mark")

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
  name = "item1-paragraph"

  paragraphs.delete_pre_draw_proc_by_name(name)
  paragraphs.delete_post_draw_proc_by_name(name)

  paragraphs.prop_set("foreground", @ruby_gnome2_color)
  space = @space * (3 / 8.0)

  paragraphs.add_post_draw_proc(name) do |paragraph, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + w, y + space, @ruby_gnome2_line_color)
    end
    [x, y, w, h]
  end
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"
  
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "black"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_width * 0.5
    canvas.draw_circle(true, x, y, w, h, color)
  end
end

match(*(slide_body + (item_list_item * 3))) do |items|
  name = "item3"
  
  mark_width = screen_x(1.0)
  mark_height = screen_y(1.0)
  indent_width = mark_width * 3
  color = "black"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_width
    canvas.draw_rectangle(true, x, y, w, h, color)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1) + [Paragraph])) do |paragraphs|
  name = "enum1-paragraph"
  
  paragraphs.delete_pre_draw_proc_by_name(name)
  paragraphs.delete_post_draw_proc_by_name(name)

  paragraphs.prop_set("foreground", @ruby_gnome2_color)
  space = @space * (3 / 8.0)

  paragraphs.add_post_draw_proc(name) do |paragraph, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + w, y + space, @ruby_gnome2_line_color)
    end
    [x, y, w, h]
  end
end
