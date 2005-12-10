slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"

  mark_width = screen_x(4)
  mark_height = screen_y(4)
  mark_space = mark_width * 0.3
  margin_left = canvas.width * 0.1 + mark_width
  indent_width = margin_left + mark_width + mark_space
    
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    sx = start_x - mark_space
    canvas.draw_circle(true, sx, start_y, end_x, end_y,
                       @blue_circle_blue)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"

  mark_width = screen_x(4)
  mark_height = screen_y(4)
  mark_space = mark_width * 0.2
  indent_width = mark_width

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  line_width = {:line_width => screen_size(0.1)}
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    sx = start_x - mark_space
    canvas.draw_circle(true, sx, start_y, end_x, end_y, "white")
    canvas.draw_circle(false, sx, start_y, end_x, end_y,
                       @blue_circle_blue, line_width)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end
