include_theme("default-item-mark-setup")

margin_left = canvas.width * 0.05

slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  compute_mark_space = Proc.new do |mark_width|
    mark_width * 0.3
  end
  compute_indent_width = Proc.new do |mark_width, mark_space|
    margin_left + mark_width * 2 + mark_space
  end
  setup_default_item_mark(items, "item1", 4, 4, (3 / 4.0), @color_circle_color,
                          "mark_space" => compute_mark_space,
                          "indent_width" => compute_indent_width,
                          "type" => "circle")
end

match(*(slide_body + (item_list_item * 2))) do |items|
  compute_mark_space = Proc.new do |mark_width|
    mark_width * 0.2
  end
  compute_indent_width = Proc.new do |mark_width, mark_space|
    mark_width
  end

  args = [items, "item2", 4, 4, (3 / 4.0), @color_circle_color,
          {
            "mark_space" => compute_mark_space,
            "indent_width" => compute_indent_width,
          }]
  line_width = {:line_width => screen_size(0.1)}
  setup_default_item_mark(*args) do |item, canvas, x, y, w, h, color|
    canvas.draw_circle(true, x, y, w, h, @color_circle_background)
    canvas.draw_circle(false, x, y, w, h, @color_circle_color, line_width)
  end
end

match(*(slide_body + (item_list_item * 3))) do |items|
  compute_mark_space = Proc.new do |mark_width|
    mark_width * 0.2
  end
  compute_indent_width = Proc.new do |mark_width, mark_space|
    mark_width
  end
  setup_default_item_mark(items, "item3", 4, 4, (3 / 4.0), @color_circle_color,
                          "mark_space" => compute_mark_space,
                          "indent_width" => compute_indent_width,
                          "type" => "circle")
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1))) do |items|
  compute_indent_width = Proc.new do |indent_width|
    margin_left + indent_width
  end
  setup_default_enum_item_mark(items, "enum1", 2, (3 / 4.0),
                               {"size" => @normal_font_size},
                               "indent_width" => compute_indent_width,
                               "type" => "numeric")
end

match(*(slide_body + (enum_list_item * 2))) do |items|
  setup_default_enum_item_mark(items, "enum2", 1.5, (2 / 4.0),
                               {"size" => @small_font_size},
                               {"type" => "lower-case"})
end

match(*(slide_body + (enum_list_item * 3))) do |items|
  setup_default_enum_item_mark(items, "enum3", 1, (1 / 4.0),
                               {"size" => @x_small_font_size},
                               {"type" => "upper-case"})
end
