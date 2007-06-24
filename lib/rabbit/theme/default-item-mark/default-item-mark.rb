@default_item1_mark_color ||= "green"
@default_item2_mark_color ||= "blue"
@default_item3_mark_color ||= "red"
@default_enum_item1_mark_color ||= "#00ffff"
@default_enum_item2_mark_color ||= "#ff00ff"

def setup_default_item_mark(items, name, width, height, space_ratio, color)
  mark_width = screen_x(width)
  mark_height = screen_y(height)
  indent_width = mark_width * 3

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)

  args = [items, indent_width, mark_width, mark_height, name]
  draw_mark(*args) do |item, canvas, x, y, w, h|
    x -= mark_width
    if block_given?
      yield(item, canvas, x, y, w, h, color)
    else
      canvas.draw_rectangle(true, x, y, w, h, color)
    end
  end

  space = @space * space_ratio
  margin_with(:bottom => space)
end

def setup_default_enum_item_mark(items, level, indent, space_ratio, props)
  name = "enum#{level}"

  indent_width = screen_x(2)
  default_props = {
    "font_family" => @font_family,
  }.merge(props)

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)

  draw_order(items, indent_width, name) do |item|
    props = default_props
    props = props.merge(item.first.text_props) unless item.empty?
    %Q[<span #{to_attrs(props)}>#{yield(item)}</span>]
  end

  space = @space * space_ratio
  margin_with(:bottom => space)
end

slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  setup_default_item_mark(items, "item1", 2, 2, (3 / 4.0),
                          @default_item1_mark_color)
end

match(*(slide_body + (item_list_item * 2))) do |items|
  setup_default_item_mark(items, "item2", 1.5, 1.5, (2 / 4.0),
                          @default_item2_mark_color)
end

match(*(slide_body + (item_list_item * 3))) do |items|
  setup_default_item_mark(items, "item3", 1, 1, (1 / 4.0),
                          @default_item3_mark_color)
end


enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1))) do |items|
  setup_default_enum_item_mark(items, 1, 2, (3 / 4.0),
                               "size" => @normal_font_size) do |item|
    "#{item.order}. "
  end
end

match(*(slide_body + (enum_list_item * 2))) do |items|
  setup_default_enum_item_mark(items, 2, 1.5, (2 / 4.0),
                               "size" => @small_font_size) do |item|
    "#{(?a + item.order - 1).chr}. "
  end
end

match(*(slide_body + (enum_list_item * 3))) do |items|
  setup_default_enum_item_mark(items, 3, 1, (1 / 4.0),
                               "size" => @x_small_font_size) do |item|
    "#{(?A + item.order - 1).chr}. "
  end
end


match(*(slide_body + enum_list_item + item_list_item)) do |items|
  setup_default_item_mark(items, "enum-item1", 1.5, 1.5, (2 / 4.0),
                          @default_enum_item1_mark_color)
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  setup_default_item_mark(items, "enum-item2", 1, 1, (1 / 4.0),
                          @default_enum_item2_mark_color)
end
