slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"
  
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "green"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    sx = start_x - mark_width
    canvas.draw_rectangle(true, sx, start_y, end_x, end_y, color)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"
  
  mark_width = screen_x(1.5)
  mark_height = screen_y(1.5)
  indent_width = mark_width * 3
  color = "blue"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    sx = start_x - mark_width
    canvas.draw_circle(true, sx, start_y, end_x, end_y, color)
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 3))) do |items|
  name = "item3"
  
  mark_width = screen_x(1.0)
  mark_height = screen_y(1.0)
  indent_width = mark_width * 3
  color = "red"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    sx = start_x - mark_width
    canvas.draw_rectangle(true, sx, start_y, end_x, end_y, color)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1))) do |items|
  name = "enum1"
  
  indent_width = screen_x(2)
  props = {
    "size" => @normal_font_size,
    "font_family" => @font_family,
  }

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    %Q[<span #{to_attrs(props)}>#{item.order}. </span>]
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (enum_list_item * 2))) do |items|
  name = "enum2"
  
  indent_width = screen_x(1.5)
  props = {
    "size" => @small_font_size,
    "font_family" => @font_family,
  }

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    %Q[<span #{to_attrs(props)}>#{(?a + item.order - 1).chr}. </span>]
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (enum_list_item * 3))) do |items|
  name = "enum3"
  
  indent_width = screen_x(1)
  props = {
    "size" => @x_small_font_size,
    "font_family" => @font_family,
  }

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    %Q[<span #{to_attrs(props)}>#{(?A + item.order - 1).chr}. </span>]
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end


match(*(slide_body + enum_list_item + item_list_item)) do |items|
  name = "enum-item1"
  
  mark_width = screen_x(1.5)
  mark_height = screen_y(1.5)
  indent_width = mark_width * 3
  color = "#00ffff"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    sx = start_x - mark_width
    canvas.draw_rectangle(true, sx, start_y, end_x, end_y, color)
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  name = "enum-item2"
  
  mark_width = screen_x(1)
  mark_height = screen_y(1)
  indent_width = mark_width * 3
  color = "#ff00ff"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    sx = start_x - mark_width
    canvas.draw_rectangle(true, sx, start_y, end_x, end_y, color)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end


desc_list_item = [DescriptionList, DescriptionListItem]

match(*(slide_body + desc_list_item)) do |items|
  name = "desc-item1"
  
  items.delete_post_draw_proc_by_name(name)
  
  space = @normal_font_size / Pango::SCALE
  items.each do |item|
    term_items = ElementContainer.new(item[1..-1])
    term_items.delete_pre_draw_proc_by_name(name)
    term_items.delete_post_draw_proc_by_name(name)
    indent(term_items, space, name)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (desc_list_item * 2))) do |items|
  name = "desc-item2"
  
  items.delete_post_draw_proc_by_name(name)
  
  space = @small_font_size / Pango::SCALE
  items.each do |item|
    term_items = ElementContainer.new(item[1..-1])
    term_items.delete_pre_draw_proc_by_name(name)
    term_items.delete_post_draw_proc_by_name(name)
    indent(term_items, space, name)
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (desc_list_item * 3))) do |items|
  name = "desc-item3"
  
  items.delete_post_draw_proc_by_name(name)
  
  space = @x_small_font_size / Pango::SCALE
  items.each do |item|
    term_items = ElementContainer.new(item[1..-1])
    term_items.delete_pre_draw_proc_by_name(name)
    term_items.delete_post_draw_proc_by_name(name)
    indent(term_items, space, name)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end
