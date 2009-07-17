include_theme("default-item-mark")

slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"
  
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "black"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_width
    canvas.draw_circle(true, x, y, w, h, color)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"
  
  mark_width = screen_x(1.5)
  mark_height = screen_y(1.5)
  indent_width = mark_width * 3
  color = "black"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_width
    canvas.draw_circle(true, x, y, w, h, color)
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
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
    canvas.draw_circle(true, x, y, w, h, color)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + enum_list_item + item_list_item)) do |items|
  name = "enum-item1"
  
  mark_width = screen_x(1.5)
  mark_height = screen_y(1.5)
  indent_width = mark_width * 3
  color = "black"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_width
    canvas.draw_circle(true, x, y, w, h, color)
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  name = "enum-item2"
  
  mark_width = screen_x(1.0)
  mark_height = screen_y(1.0)
  indent_width = mark_width * 3
  color = "black"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_width
    canvas.draw_circle(true, x, y, w, h, color)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end
