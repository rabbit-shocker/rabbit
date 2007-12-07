include_theme("default-item-mark-setup")

@default_item1_mark_color ||= "green"
@default_item2_mark_color ||= "blue"
@default_item3_mark_color ||= "red"
@default_enum_item1_mark_color ||= "#00ffff"
@default_enum_item2_mark_color ||= "#ff00ff"
@default_desc_item1_mark_color ||= "green"

@default_item1_mark_type ||= "rectangle"
@default_item2_mark_type ||= "rectangle"
@default_item3_mark_type ||= "rectangle"
@default_enum_item1_mark_type ||= "circle"
@default_enum_item2_mark_type ||= "circle"
@default_desc_item1_mark_type ||= "rectangle"

slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  setup_default_item_mark(items, "item1", 2, 2, (3 / 4.0),
                          @default_item1_mark_color,
                          "type" => @default_item1_mark_type)
end

match(*(slide_body + (item_list_item * 2))) do |items|
  setup_default_item_mark(items, "item2", 1.5, 1.5, (2 / 4.0),
                          @default_item2_mark_color,
                          "type" => @default_item2_mark_type)
end

match(*(slide_body + (item_list_item * 3))) do |items|
  setup_default_item_mark(items, "item3", 1, 1, (1 / 4.0),
                          @default_item3_mark_color,
                          "type" => @default_item3_mark_type)
end


enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1))) do |items|
  setup_default_enum_item_mark(items, "enum1", 2, (3 / 4.0),
                               {"size" => @normal_font_size},
                               {"type" => "numeric"})
end

match(*(slide_body + (enum_list_item * 2))) do |items|
  setup_default_enum_item_mark(items, "enum2", 1.5, (2 / 4.0),
                               {"size" => @small_font_size},
                               {"type" => "lower-case"})
end

match(*(slide_body + (enum_list_item * 3))) do |items|
  setup_default_enum_item_mark(items, 3, 1, (1 / 4.0),
                               {"size" => @x_small_font_size},
                               {"type" => "upper-case"})
end


match(*(slide_body + enum_list_item + item_list_item)) do |items|
  setup_default_item_mark(items, "enum-item1", 1.5, 1.5, (2 / 4.0),
                          @default_enum_item1_mark_color,
                          "type" => @default_enum_item1_mark_type)
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  setup_default_item_mark(items, "enum-item2", 1, 1, (1 / 4.0),
                          @default_enum_item2_mark_color,
                          "type" => @default_enum_item2_mark_type)
end


desc_list_content = [DescriptionList, DescriptionListItem, DescriptionContent]

match(*(slide_body + desc_list_content + item_list_item)) do |items|
  setup_default_item_mark(items, "desc-item1", 1.5, 1.5, (2 / 4.0),
                          @default_desc_item1_mark_color,
                          "type" => @default_desc_item1_mark_type)
end
