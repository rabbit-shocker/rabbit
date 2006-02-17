margin_left = canvas.width * 0.05

slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"

  mark_width = screen_x(4)
  mark_height = screen_y(4)
  mark_space = mark_width * 0.3
  indent_width = margin_left + mark_width * 2 + mark_space

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_space
    canvas.draw_circle(true, x, y, w, h, @blue_circle_blue)
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
    |item, canvas, x, y, w, h|
    x -= mark_space
    canvas.draw_circle(true, x, y, w, h, "white")
    canvas.draw_circle(false, x, y, w, h, @blue_circle_blue, line_width)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 3))) do |items|
  name = "item3"

  mark_width = screen_x(4)
  mark_height = screen_y(4)
  mark_space = mark_width * 0.2
  indent_width = mark_width

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  line_width = {:line_width => screen_size(0.1)}
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_space
    canvas.draw_circle(true, x, y, w, h, @blue_circle_blue)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1))) do |items|
  name = "enum1"
  
  indent_width = margin_left + screen_x(2)
  default_props = {
    "size" => @normal_font_size,
    "font_family" => @font_family,
  }

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    props = default_props
    props = props.merge(item.first.text_props) unless item.empty?
    %Q[<span #{to_attrs(props)}>#{item.order}. </span>]
  end

  space = @space * (3 / 4.0)
  margin_with(:bottom => space)
end

match(*(slide_body + (enum_list_item * 2))) do |items|
  name = "enum2"
  
  indent_width = screen_x(1.5)
  default_props = {
    "size" => @small_font_size,
    "font_family" => @font_family,
  }

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    props = default_props
    props = props.merge(item.first.text_props) unless item.empty?
    %Q[<span #{to_attrs(props)}>#{(?a + item.order - 1).chr}. </span>]
  end

  space = @space * (2 / 4.0)
  margin_with(:bottom => space)
end

match(*(slide_body + (enum_list_item * 3))) do |items|
  name = "enum3"
  
  indent_width = screen_x(1)
  default_props = {
    "size" => @x_small_font_size,
    "font_family" => @font_family,
  }

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    props = default_props
    props = props.merge(item.first.text_props) unless item.empty?
    %Q[<span #{to_attrs(props)}>#{(?A + item.order - 1).chr}. </span>]
  end

  space = @space * (1 / 4.0)
  margin_with(:bottom => space)
end
